use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::ToTokens;
use syn::{parse_macro_input, Item};

#[proc_macro_attribute]
pub fn sorted(_args: TokenStream, input: TokenStream) -> TokenStream {
    let item = parse_macro_input!(input as Item);
    let mut output = item.to_token_stream();
    // let output =

    if let Err(e) = handle_item(item) {
        output.extend(syn::Error::new(Span::call_site(), e).into_compile_error())
    }
    output.into()
}

fn handle_item(item: syn::Item) -> Result<(), &'static str> {
    match item {
        Item::Enum(_) => Ok(()),
        _ => Err("expected enum or match expression"),
    }
}
