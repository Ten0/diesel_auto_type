use super::*;

impl<'a> LocalVariablesMap<'a> {
	pub(crate) fn inferrer(&'a self) -> TypeInferrer<'a> {
		TypeInferrer {
			local_variables_map: self,
			errors: Default::default(),
		}
	}
}

pub(crate) struct TypeInferrer<'s> {
	local_variables_map: &'s LocalVariablesMap<'s>,
	errors: std::cell::RefCell<Vec<Rc<syn::Error>>>,
}

impl TypeInferrer<'_> {
	pub(crate) fn infer_hinted_expression_type(&self, expr: &syn::Expr, hint: &syn::Type) -> syn::Type {
		match hint {
			syn::Type::Infer(_) => self.infer_expression_type(expr),
			syn::Type::Tuple(type_tuple @ syn::TypeTuple { elems: type_elems, .. }) => match expr {
				syn::Expr::Tuple(syn::ExprTuple { elems: expr_elems, .. }) => {
					if type_elems.len() != expr_elems.len() {
						return self.register_error(syn::Error::new(
							type_tuple.span(),
							"auto_type: tuple type and its \
									expression have different number of elements",
						));
					}
					syn::Type::Tuple(syn::TypeTuple {
						elems: type_elems
							.iter()
							.zip(expr_elems.iter())
							.map(|(type_, expr)| self.infer_hinted_expression_type(expr, type_))
							.collect(),
						..type_tuple.clone()
					})
				}
				_ => hint.clone(),
			},
			_ => hint.clone(),
		}
	}
	/// Calls `try_infer_expression_type` and falls back to `_` if it fails, collecting the error
	/// for display
	pub(crate) fn infer_expression_type(&self, expr: &syn::Expr) -> syn::Type {
		let inferred = self.try_infer_expression_type(expr);

		match inferred {
			Ok(t) => t,
			Err(e) => self.register_error(e),
		}
	}
	fn register_error(&self, error: syn::Error) -> syn::Type {
		self.errors.borrow_mut().push(Rc::new(error));
		parse_quote!(_)
	}
	fn try_infer_expression_type(&self, expr: &syn::Expr) -> Result<syn::Type, syn::Error> {
		let expression_type: syn::Type = match expr {
			syn::Expr::Path(syn::ExprPath { path, .. }) => {
				// This is either a local variable or we should assume that the type exists at the same path as the
				// function
				if let Some(LetStatementInferredType { type_, errors }) = path
					.get_ident()
					.and_then(|path_single_ident| self.local_variables_map.map.get(path_single_ident))
				{
					// Since we are using this type for the computation of the current type, any errors encountered
					// there are relevant here
					self.errors.borrow_mut().extend(errors.iter().cloned());
					type_.clone()
				} else {
					syn::Type::Path(syn::TypePath {
						path: path.clone(),
						qself: None,
					})
				}
			}
			syn::Expr::Call(syn::ExprCall { func, args, .. }) => {
				let unsupported_function_type =
					|| syn::Error::new_spanned(&**func, "unsupported function type for auto_type");
				let func_type = self.try_infer_expression_type(func)?;
				// First we extract the type of the function
				let mut type_path = match func_type {
					syn::Type::Path(syn::TypePath { path, .. }) => path,
					_ => return Err(unsupported_function_type()),
				};
				// Then we will add the generic arguments
				let last_segment = type_path.segments.last_mut().ok_or_else(unsupported_function_type)?;
				last_segment.arguments = self.infer_generics_or_use_hints(
					None,
					args,
					match &last_segment.arguments {
						syn::PathArguments::None => None,
						syn::PathArguments::AngleBracketed(ab) => Some(ab),
						syn::PathArguments::Parenthesized(_) => return Err(unsupported_function_type()),
					},
				)?;
				syn::Type::Path(syn::TypePath {
					path: type_path,
					qself: None,
				})
			}
			syn::Expr::MethodCall(syn::ExprMethodCall {
				receiver,
				method,
				turbofish,
				args,
				..
			}) => syn::Type::Path(syn::TypePath {
				path: syn::Path {
					segments: self
						.local_variables_map
						.inferrer_settings
						.dsl_path
						.segments
						.iter()
						.cloned()
						.chain([syn::PathSegment {
							ident: Ident::new(
								&heck::ToPascalCase::to_pascal_case(method.to_string().as_str()),
								method.span(),
							),
							arguments: self.infer_generics_or_use_hints(
								Some(syn::GenericArgument::Type(self.infer_expression_type(receiver))),
								args,
								turbofish.as_ref(),
							)?,
						}])
						.collect(),
					leading_colon: None,
				},
				qself: None,
			}),
			syn::Expr::Tuple(syn::ExprTuple { elems, .. }) => syn::Type::Tuple(syn::TypeTuple {
				elems: elems.iter().map(|e| self.infer_expression_type(e)).collect(),
				paren_token: Default::default(),
			}),
			syn::Expr::Lit(syn::ExprLit { lit, .. }) => match lit {
				syn::Lit::Str(_) => parse_quote!(&'static str),
				syn::Lit::ByteStr(_) => parse_quote!(&'static [u8]),
				syn::Lit::Byte(_) => parse_quote!(u8),
				syn::Lit::Char(_) => parse_quote!(char),
				syn::Lit::Int(lit_int) => litteral_type(&lit_int.token())?,
				syn::Lit::Float(lit_float) => litteral_type(&lit_float.token())?,
				syn::Lit::Bool(_) => parse_quote!(bool),
				_ => return Err(syn::Error::new(lit.span(), "unsupported literal for auto_type")),
			},
			_ => return Err(syn::Error::new(expr.span(), "unsupported expression for auto_type")),
		};
		Ok(expression_type)
	}

	fn infer_generics_or_use_hints(
		&self,
		add_first: Option<syn::GenericArgument>,
		args: &syn::punctuated::Punctuated<syn::Expr, Token![,]>,
		hint: Option<&syn::AngleBracketedGenericArguments>,
	) -> Result<syn::PathArguments, syn::Error> {
		let arguments = syn::AngleBracketedGenericArguments {
			args: add_first
				.into_iter()
				.chain(match hint {
					None => {
						// We should infer
						Either::Left(
							args.iter()
								.map(|e| syn::GenericArgument::Type(self.infer_expression_type(e))),
						)
					}
					Some(hint_or_override) => Either::Right({
						let hints_none_if_infer = hint_or_override.args.iter().map(|ga| match ga {
							syn::GenericArgument::Type(syn::Type::Infer(_)) => None,
							other => Some(other),
						});
						if hints_none_if_infer.clone().any(|arg| arg.is_none()) {
							// This is only partially hinted: we know how many generics there are, but there are some
							// underscores. We need to infer those.
							Either::Left(hints_none_if_infer.zip(args.iter()).map(|(hint, expr)| match hint {
								Some(hint) => hint.clone(),
								None => syn::GenericArgument::Type(self.infer_expression_type(expr)),
							}))
						} else {
							// No underscores, we can just take the provided arguments
							Either::Right(hint_or_override.args.iter().cloned())
						}
					}),
				})
				.collect(),
			colon2_token: None, // there is no colon2 in types, only in function calls
			lt_token: Default::default(),
			gt_token: Default::default(),
		};
		Ok(if arguments.args.is_empty() {
			syn::PathArguments::None
		} else {
			syn::PathArguments::AngleBracketed(arguments)
		})
	}

	pub(crate) fn into_errors(self) -> Vec<Rc<syn::Error>> {
		self.errors.into_inner()
	}
}
