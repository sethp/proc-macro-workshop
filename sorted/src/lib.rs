use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::ToTokens;
use syn::{
    parse_macro_input, visit_mut::VisitMut, Attribute, Item, ItemFn, Pat, PathArguments,
    PathSegment, Variant,
};

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
    fn pat_path(pat: &Pat) -> Result<Ordered, syn::Error> {
        match pat {
            Pat::TupleStruct(ts) => Ok(Ordered(ts.path.clone())),
            _ => todo!("{:?}", pat),
        }
    }

    let arms = mexpr.arms.as_slice();
    let (first, rest) = match arms {
        &[] | &[_] => return Ok(()),
        &[ref first, ref rest @ ..] => (pat_path(&first.pat)?, rest),
    };

    let mut seen = vec![first];
    for arm in rest {
        let ident = pat_path(&arm.pat)?;
        if seen.last().unwrap() < &ident {
            seen.push(ident);
            continue;
        }

        let before = seen.iter().find(|&e| &ident < e).unwrap();
        return Err(syn::Error::new_spanned(
            &ident.0,
            format!("{} should sort before {}", ident, before),
        ));
    }

    Ok(())
}

#[derive(Debug)]
struct Ordered(syn::Path);

impl std::fmt::Display for Ordered {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}{}",
            if let Some(_) = self.0.leading_colon {
                "::"
            } else {
                ""
            },
            self.0
                .segments
                .iter()
                .map(|s| match s.arguments {
                    PathArguments::None => s.ident.to_string(),
                    _ => unimplemented!(),
                })
                .collect::<Vec<String>>()
                .join("::")
        )
    }
}

impl Ord for Ordered {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        use std::cmp::Ordering;

        match (self.0.leading_colon, other.0.leading_colon) {
            (Some(_), None) => return Ordering::Less,
            (None, Some(_)) => return Ordering::Greater,
            (Some(_), Some(_)) | (None, None) => (),
        };

        let mut ss = self.0.segments.iter();
        let mut os = other.0.segments.iter();

        loop {
            match (ss.next(), os.next()) {
                (
                    Some(PathSegment {
                        ident: s,
                        arguments: PathArguments::None,
                    }),
                    Some(PathSegment {
                        ident: o,
                        arguments: PathArguments::None,
                    }),
                ) => {
                    if s < o {
                        return Ordering::Less;
                    } else if s > o {
                        return Ordering::Greater;
                    }
                }
                (Some(_), None) => return Ordering::Greater,
                (None, Some(_)) => return Ordering::Less,
                (None, None) => break,
                _ => unimplemented!(),
            };
        }

        Ordering::Equal
    }
}

impl PartialOrd for Ordered {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq for Ordered {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other).is_eq()
    }
}

impl Eq for Ordered {}

#[cfg(test)]
pub mod test {
    use syn::parse_quote;

    use crate::Ordered;

    #[test]
    fn path_ordering() {
        // might be nice to quickcheck this
        let mut all = [
            parse_quote!(a),
            parse_quote!(a::a),
            parse_quote!(a::b),
            parse_quote!(b::a),
            parse_quote!(::a),
            parse_quote!(::a::b),
            parse_quote!(b),
        ]
        .map(Ordered);

        all.sort();

        assert_eq!(
            [
                // comments keep these rustfmt'd on separate lines
                "::a",    //
                "::a::b", //
                "a",      //
                "a::a",   //
                "a::b",   //
                "b",      //
                "b::a",   //
            ],
            all.map(|o| format!("{}", o))
        );
    }

    // fn path_to_string(p: Path) -> String {}
}
