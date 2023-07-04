mod case;
mod expression_type_inference;
mod local_variables_map;

use {
	darling::{util::SpannedValue, FromMeta},
	either::Either,
	proc_macro2::TokenStream,
	quote::quote,
	std::{collections::HashMap, rc::Rc},
	syn::{parse_quote, spanned::Spanned, Ident, ItemFn, Token, Type},
};

use {case::*, local_variables_map::*};

#[derive(darling::FromMeta)]
struct Settings {
	/// Can be overridden to provide custom DSLs or directly refer to `diesel::dsl`
	dsl_path: Option<syn::Path>,
	type_alias: darling::util::Flag,
	type_name: Option<syn::Ident>,
	type_case: Option<SpannedValue<String>>,
}

pub(crate) struct InferrerSettings {
	dsl_path: syn::Path,
}

pub(crate) fn auto_type_impl(attr: TokenStream, input: &TokenStream) -> Result<TokenStream, crate::Error> {
	let settings_input: Settings = Settings::from_list(&darling::ast::NestedMeta::parse_meta_list(attr)?)?;

	let mut input_function = syn::parse2::<ItemFn>(input.clone())?;

	let inferrer_settings = InferrerSettings {
		dsl_path: settings_input.dsl_path.unwrap_or_else(|| parse_quote!(dsl)),
	};

	let function_name = &input_function.sig.ident;
	let type_alias: Option<syn::Ident> = match (
		settings_input.type_alias.is_present(),
		settings_input.type_name,
		settings_input.type_case,
	) {
		(false, None, None) => None,
		(true, None, None) => {
			// We have to use snake as the default to be consistent with call expressions assuming that the type
			// path is equal to the function name, as that is the convention in Diesel's `helper_types`
			Some(Case::SnakeCase.ident_with_case(function_name))
		}
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
	let (return_type, errors) = match input_function.sig.output {
		syn::ReturnType::Type(_, return_type) if matches!(*return_type, Type::Infer(_)) => {
			// Let's process intermediate let statements

			let mut local_variables_map = LocalVariablesMap {
				inferrer_settings: &inferrer_settings,
				map: Default::default(),
			};
			for function_param in &input_function.sig.inputs {
				match function_param {
					syn::FnArg::Typed(syn::PatType { pat, ty, .. }) => {
						local_variables_map.process_pat(pat, Some(ty), None)?
					}
					_ => {}
				};
			}
			for statement in &input_function.block.stmts[0..input_function.block.stmts.len() - 1] {
				match statement {
					syn::Stmt::Local(local) => local_variables_map.process_pat(
						&local.pat,
						None,
						local.init.as_ref().map(|local_init| &*local_init.expr),
					)?,
					_ => {}
				};
			}

			// Now we can process the last statement
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
			let inferrer = local_variables_map.inferrer();
			(
				inferrer.infer_expression_type(return_expression, None),
				inferrer.into_errors(),
			)
		}
		syn::ReturnType::Type(_, return_type) if type_alias.is_some() => {
			// User only wants us to generate a type alias for the return type but we don't have anything to infer
			(*return_type, Vec::new())
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

	for error in errors {
		// Extracting from the `Rc` only if it's the last reference is an elegant way to deduplicate errors
		// For this to work it is necessary that the rest of the errors (those from the local variables map that weren't
		// used) are dropped before, which is the case here, and that we are iterating on the errors in an owned manner.
		if let Ok(error) = Rc::try_unwrap(error) {
			res.extend(error.into_compile_error());
		}
	}

	Ok(res)
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
