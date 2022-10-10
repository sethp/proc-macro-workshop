use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::ToTokens;
use syn::{parse_macro_input, visit_mut::VisitMut, Attribute, Ident, Item, ItemFn, Pat, Variant};

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
    let e = match item {
        Item::Enum(e) => e,
        _ => {
            return Err(syn::Error::new(
                Span::call_site(),
                "expected enum or match expression",
            ))
        }
    };

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
            .find(|v| ident < &v.ident)
            .unwrap() // ident must be < at least "last", but may come sooner
            .ident;
        return Err(syn::Error::new_spanned(
            ident,
            format!("{} should sort before {}", ident, before),
        ));
    }

    Ok(())
}

#[proc_macro_attribute]
pub fn check(_args: TokenStream, input: TokenStream) -> TokenStream {
    let mut item_fn = parse_macro_input!(input as ItemFn);

    let mut visitor = Sorted::default();
    visitor.visit_item_fn_mut(&mut item_fn);

    let mut out = item_fn.to_token_stream();
    out.extend(visitor.0.iter().map(syn::Error::to_compile_error));
    out.into()
}

#[derive(Default)]
struct Sorted(Vec<syn::Error>);

impl syn::visit_mut::VisitMut for Sorted {
    fn visit_expr_match_mut(&mut self, mexpr: &mut syn::ExprMatch) {
        if let Some(idx) = mexpr
            .attrs
            .iter()
            .position(|&Attribute { ref path, .. }| path.is_ident("sorted"))
        {
            mexpr.attrs.remove(idx);

            self.0.extend(handle_match_expr(mexpr).err());
        }
    }
}

fn handle_match_expr(mexpr: &syn::ExprMatch) -> Result<(), syn::Error> {
    fn pat_ident(pat: &Pat) -> Result<&Ident, syn::Error> {
        match pat {
            Pat::Ident(i) => Ok(&i.ident),
            Pat::TupleStruct(ts) => ts.path.get_ident().ok_or_else(|| todo!()),
            _ => todo!("{:?}", pat),
        }
    }

    let arms = mexpr.arms.as_slice();
    let (first, rest) = match arms {
        &[] | &[_] => return Ok(()),
        &[ref first, ref rest @ ..] => (pat_ident(&first.pat)?, rest),
    };

    let mut seen = vec![first];
    for arm in rest {
        let ident = pat_ident(&arm.pat)?;
        if seen.last().unwrap() < &ident {
            seen.push(ident);
            continue;
        }

        let before = seen.iter().find(|e| ident < e).unwrap();
        return Err(syn::Error::new_spanned(
            ident,
            format!("{} should sort before {}", ident, before),
        ));
    }

    Ok(())
}
