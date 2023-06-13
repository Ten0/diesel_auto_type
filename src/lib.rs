mod auto_type;

enum Error {
	Syn(syn::Error),
	Darling(darling::Error),
}

#[proc_macro_attribute]
pub fn auto_type(attr: proc_macro::TokenStream, input: proc_macro::TokenStream) -> proc_macro::TokenStream {
	let attr = proc_macro2::TokenStream::from(attr);
	let input = proc_macro2::TokenStream::from(input);
	match auto_type::auto_type_impl(attr, &input) {
		Ok(token_stream_1) => token_stream_1.into(),
		Err(e) => {
			let mut out = input;
			match e {
				Error::Syn(e) => {
					out.extend(e.into_compile_error());
				}
				Error::Darling(e) => {
					out.extend(e.write_errors());
				}
			}
			out.into()
		}
	}
}

impl From<syn::Error> for Error {
	fn from(e: syn::Error) -> Self {
		Error::Syn(e)
	}
}
impl From<darling::Error> for Error {
	fn from(e: darling::Error) -> Self {
		Error::Darling(e)
	}
}
