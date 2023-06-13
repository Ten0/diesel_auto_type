use {
	darling::FromMeta,
	either::Either,
	proc_macro2::{Span, TokenStream},
	quote::quote,
	syn::{parse_quote, spanned::Spanned, Ident, ItemFn, Type},
};

#[derive(darling::FromMeta)]
struct Expander {
	/// Can be overridden to provide custom DSLs or directly refer to `diesel::dsl`
	dsl_path: Option<syn::Path>,
	// TODO add inline param
}

pub(crate) fn auto_type_impl(attr: TokenStream, input: &TokenStream) -> Result<TokenStream, crate::Error> {
	let expander: Expander = Expander::from_list(&darling::ast::NestedMeta::parse_meta_list(attr)?)?;

	let mut input_function = syn::parse2::<ItemFn>(input.clone())?;
	let last_statement = input_function
		.block
		.stmts
		.last()
		.ok_or_else(|| syn::Error::new(input_function.span(), "function body should not be empty for auto_type"))?;
	match input_function.sig.output {
		syn::ReturnType::Type(_, return_type) if matches!(*return_type, Type::Infer(_)) => {}
		_ => {
			return Err(syn::Error::new(
				input_function.sig.output.span(),
				"Function return type should be explicitly specified as `-> _` for auto_type",
			)
			.into())
		}
	}
	let return_expression = match last_statement {
		syn::Stmt::Expr(expr, None) => expr,
		syn::Stmt::Expr(syn::Expr::Return(syn::ExprReturn { expr: Some(expr), .. }), _) => &**expr,
		_ => {
			return Err(syn::Error::new(
				last_statement.span(),
				"last statement should be an expression for auto_type",
			)
			.into())
		}
	};
	let return_type = expander.infer_expression_type(return_expression)?;
	input_function.sig.output = parse_quote!(-> #return_type);
	Ok(quote! {
		#input_function
	})
}

impl Expander {
	fn infer_expression_type(&self, expr: &syn::Expr) -> Result<syn::Type, syn::Error> {
		let expression_type: syn::Type = match expr {
			syn::Expr::Path(syn::ExprPath { path, .. }) => syn::Type::Path(syn::TypePath {
				path: path.clone(),
				qself: None,
			}),
			syn::Expr::Call(syn::ExprCall { func, args, .. }) => {
				let unsupported_function_type =
					|| syn::Error::new_spanned(&**func, "unsupported function type for auto_type");
				let func_type = self.infer_expression_type(func)?;
				// First we extract the type of the function
				let mut type_path = match func_type {
					syn::Type::Path(syn::TypePath { path, .. }) => path,
					_ => return Err(unsupported_function_type()),
				};
				// Then we will add the generic arguments
				let last_segment = type_path.segments.last_mut().ok_or_else(unsupported_function_type)?;
				last_segment.arguments = syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments {
					args: match &mut last_segment.arguments {
						syn::PathArguments::None => {
							// We should infer
							args.iter()
								.map(|e| self.infer_expression_type(e).map(syn::GenericArgument::Type))
								.collect::<Result<_, _>>()?
						}
						syn::PathArguments::AngleBracketed(ab) => {
							// TODO if there are underscores in there we should still infer
							std::mem::take(&mut ab.args)
						}
						syn::PathArguments::Parenthesized(_) => return Err(unsupported_function_type()),
					},
					colon2_token: None, // there is no colon2 in types, only in function calls
					lt_token: Default::default(),
					gt_token: Default::default(),
				});
				//last_segment.arguments
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
					segments: (match &self.dsl_path {
						None => Either::Left(
							[syn::PathSegment {
								ident: Ident::new("dsl", Span::call_site()),
								arguments: syn::PathArguments::None,
							}]
							.into_iter(),
						),
						Some(dsl_path) => Either::Right(dsl_path.segments.iter().cloned()),
					})
					.chain([syn::PathSegment {
						ident: Ident::new(
							&heck::ToPascalCase::to_pascal_case(method.to_string().as_str()),
							method.span(),
						),
						arguments: syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments {
							args: {
								std::iter::once(self.infer_expression_type(receiver).map(syn::GenericArgument::Type))
									.chain(match turbofish {
										Some(angle_bracketed_arguments) => {
											Either::Left(angle_bracketed_arguments.args.iter().cloned().map(Ok))
										}
										None => Either::Right(
											args.iter()
												.map(|e| self.infer_expression_type(e).map(syn::GenericArgument::Type)),
										),
									})
									.collect::<Result<_, _>>()?
							},
							colon2_token: None,
							lt_token: Default::default(),
							gt_token: Default::default(),
						}),
					}])
					.collect(),
					leading_colon: None,
				},
				qself: None,
			}),
			syn::Expr::Tuple(syn::ExprTuple { elems, .. }) => syn::Type::Tuple(syn::TypeTuple {
				elems: elems
					.iter()
					.map(|e| self.infer_expression_type(e))
					.collect::<Result<_, _>>()?,
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
}

fn litteral_type(t: &proc_macro2::Literal) -> Result<syn::Type, syn::Error> {
	let val = t.to_string();
	let type_suffix = &val[val
		.find(|c: char| !c.is_ascii_digit())
		.ok_or_else(|| syn::Error::new_spanned(t, "Litterals must have type suffix for auto_type, e.g. 15u32"))?..];
	syn::parse_str(type_suffix).map_err(|_| syn::Error::new_spanned(t, "Invalid type suffix for litteral"))
}
