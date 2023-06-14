use {heck::*, proc_macro2::Span, syn::Ident};

pub(crate) enum Case {
	UpperCamelCase,
	PascalCase,
	LowerCamelCase,
	SnakeCase,
	ShoutySnakeCase,
}

impl Case {
	pub(crate) fn ident_with_case(self, ident: &Ident) -> syn::Ident {
		let s = ident.to_string();
		let cased_s = match self {
			Case::UpperCamelCase => s.to_upper_camel_case(),
			Case::PascalCase => s.to_pascal_case(),
			Case::LowerCamelCase => s.to_lower_camel_case(),
			Case::SnakeCase => s.to_snake_case(),
			Case::ShoutySnakeCase => s.to_shouty_snake_case(),
		};
		Ident::new(&cased_s, ident.span())
	}
}

impl Case {
	pub(crate) fn from_str(s: &str, span: Span) -> Result<Self, syn::Error> {
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
