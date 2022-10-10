use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::ToTokens;
use syn::{parse_macro_input, Item, Variant};

#[proc_macro_attribute]
pub fn sorted(_args: TokenStream, input: TokenStream) -> TokenStream {
    let item = parse_macro_input!(input as Item);
    let mut output = item.to_token_stream();

    if let Err(e) = handle_item(item) {
        output.extend(e.into_compile_error())
    }
    output.into()
}

fn handle_item(item: syn::Item) -> Result<(), syn::Error> {
    match item {
        Item::Enum(e) => handle_enum(e),
        _ => Err(syn::Error::new(
            Span::call_site(),
            "expected enum or match expression",
        )),
    }
}

fn handle_enum(e: syn::ItemEnum) -> Result<(), syn::Error> {
    let mut last = match e.variants.first() {
        Some(v) => &v.ident,
        None => return Ok(()), // empty enums are sorted
    };
    for &Variant { ref ident, .. } in e.variants.iter().skip(1) {
        if last < ident {
            last = ident;
            continue;
        }

        let before = &e
            .variants
            .iter()
            .filter(|v| ident < &v.ident)
            .next()
            .unwrap() // ident must be < at least "last", but may come sooner
            .ident;
        return Err(syn::Error::new_spanned(
            ident,
            format!("{} should sort before {}", ident, before),
        ));
    }

    Ok(())
}
