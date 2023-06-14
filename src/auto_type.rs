use {
	darling::{util::SpannedValue, FromMeta},
	either::Either,
	proc_macro2::{Span, TokenStream},
	quote::quote,
	syn::{parse_quote, spanned::Spanned, Ident, ItemFn, Token, Type},
};

#[derive(darling::FromMeta)]
struct ExpanderSettings {
	/// Can be overridden to provide custom DSLs or directly refer to `diesel::dsl`
	dsl_path: Option<syn::Path>,
	type_alias: darling::util::Flag,
	type_name: Option<syn::Ident>,
	type_case: Option<SpannedValue<String>>,
}

pub(crate) fn auto_type_impl(attr: TokenStream, input: &TokenStream) -> Result<TokenStream, crate::Error> {
	let expander_settings: ExpanderSettings =
		ExpanderSettings::from_list(&darling::ast::NestedMeta::parse_meta_list(attr)?)?;

	let mut input_function = syn::parse2::<ItemFn>(input.clone())?;

	let dsl_path = expander_settings.dsl_path.unwrap_or_else(|| parse_quote!(dsl));
	let expander = Expander {
		dsl_path,
		errors: Default::default(),
	};

	let function_name = &input_function.sig.ident;
	let type_alias: Option<syn::Ident> = match (
		expander_settings.type_alias.is_present(),
		expander_settings.type_name,
		expander_settings.type_case,
	) {
		(false, None, None) => None,
		(true, None, None) => Some(Case::SnakeCase.ident_with_case(function_name)),
		(_, Some(ident), None) => Some(ident),
		(_, None, Some(case)) => {
			let case = Case::from_str(case.as_str(), case.span())?;
			Some(case.ident_with_case(function_name))
		}
		(_, Some(_), Some(type_case)) => {
			return Err(syn::Error::new(type_case.span(), "type_name and type_case are mutually exclusive").into())
		}
	};

	let last_statement = input_function
		.block
		.stmts
		.last()
		.ok_or_else(|| syn::Error::new(input_function.span(), "function body should not be empty for auto_type"))?;
	let return_type = match input_function.sig.output {
		syn::ReturnType::Type(_, return_type) if matches!(*return_type, Type::Infer(_)) => {
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
			expander.infer_expression_type(return_expression)
		}
		syn::ReturnType::Type(_, return_type) if type_alias.is_some() => {
			// User only wants us to generate a type alias for the return type but we don't have anything to infer
			*return_type
		}
		_ => {
			return Err(syn::Error::new(
				input_function.sig.output.span(),
				"Function return type should be explicitly specified as `-> _` for auto_type",
			)
			.into())
		}
	};

	let type_alias = match type_alias {
		Some(type_alias) => {
			let vis = &input_function.vis;
			input_function.sig.output = parse_quote!(-> #type_alias);
			quote! {
				#[allow(non_camel_case_types)]
				#vis type #type_alias = #return_type;
			}
		}
		None => {
			input_function.sig.output = parse_quote!(-> #return_type);
			quote! {}
		}
	};

	let mut res = quote! {
		#type_alias
		#input_function
	};

	for error in expander.errors.into_inner() {
		res.extend(error.into_compile_error());
	}

	Ok(res)
}

struct Expander {
	dsl_path: syn::Path,
	errors: std::cell::RefCell<Vec<syn::Error>>,
}

impl Expander {
	/// Calls `try_infer_expression_type` and falls back to `_` if it fails, collecting the error
	/// for display
	fn infer_expression_type(&self, expr: &syn::Expr) -> syn::Type {
		let inferred = self.try_infer_expression_type(expr);

		match inferred {
			Ok(t) => t,
			Err(e) => {
				self.errors.borrow_mut().push(e);
				parse_quote!(_)
			}
		}
	}
	fn try_infer_expression_type(&self, expr: &syn::Expr) -> Result<syn::Type, syn::Error> {
		let expression_type: syn::Type = match expr {
			syn::Expr::Path(syn::ExprPath { path, .. }) => syn::Type::Path(syn::TypePath {
				path: path.clone(),
				qself: None,
			}),
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
}

fn litteral_type(t: &proc_macro2::Literal) -> Result<syn::Type, syn::Error> {
	let val = t.to_string();
	let type_suffix = &val[val.find(|c: char| !c.is_ascii_digit()).ok_or_else(|| {
		syn::Error::new_spanned(
			t,
			format_args!("Litterals must have type suffix for auto_type, e.g. {val}i64"),
		)
	})?..];
	syn::parse_str(type_suffix).map_err(|_| syn::Error::new_spanned(t, "Invalid type suffix for litteral"))
}

enum Case {
	UpperCamelCase,
	PascalCase,
	LowerCamelCase,
	SnakeCase,
	ShoutySnakeCase,
}

impl Case {
	fn ident_with_case(self, ident: &Ident) -> syn::Ident {
		let s = ident.to_string();
		let s = s.as_str();
		let cased_s = match self {
			Case::UpperCamelCase => heck::ToUpperCamelCase::to_upper_camel_case(s),
			Case::PascalCase => heck::ToPascalCase::to_pascal_case(s),
			Case::LowerCamelCase => heck::ToLowerCamelCase::to_lower_camel_case(s),
			Case::SnakeCase => heck::ToSnakeCase::to_snake_case(s),
			Case::ShoutySnakeCase => heck::ToShoutySnakeCase::to_shouty_snake_case(s),
		};
		Ident::new(&cased_s, ident.span())
	}
}

impl Case {
	fn from_str(s: &str, span: Span) -> Result<Self, syn::Error> {
		Ok(match s {
			"UpperCamelCase" => Case::UpperCamelCase,
			"PascalCase" => Case::PascalCase,
			"lowerCamelCase" => Case::LowerCamelCase,
			"snake_case" => Case::SnakeCase,
			"SHOUTY_SNAKE_CASE" => Case::ShoutySnakeCase,
			other => {
				return Err(syn::Error::new(
					span,
					format_args!(
						"Unknown case: {other}, expected one of: \
							`PascalCase`, `snake_case`, `UpperCamelCase`, `lowerCamelCase`, `SHOUTY_SNAKE_CASE`"
					),
				))
			}
		})
	}
}
