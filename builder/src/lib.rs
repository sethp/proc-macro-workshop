use std::vec;

use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::{format_ident, quote};
use syn::{
    ext::IdentExt, parse::Parse, parse_macro_input, Attribute, Data, DataStruct, DeriveInput,
    Error, Field, Fields, GenericArgument, Ident, Lit, Path, PathArguments, PathSegment, Token,
    Type, TypePath,
};

enum BuilderAttr {
    Each(Ident),
}

impl syn::parse::Parse for BuilderAttr {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(Ident::peek_any) {
            let ident: Ident = input.parse()?;
            if ident != "each" {
                return Err(input.error("expected: `each`"));
            }

            let _: Token![=] = input.parse()?;
            // let lit: Literal = input.parse()?;
            let arg = match input.parse()? {
                Lit::Str(str) => str.value(),
                _ => unimplemented!(),
            };

            return Ok(BuilderAttr::Each(Ident::new(&arg, Span::call_site())));
        }

        Err(lookahead.error())
    }
}

// quick review:
//  1. hard-to-support, but likely-to-desire features include:
//      1. supporting more ergonomics for "known" types like Option
//      2. adding more attributes (+values) to expand functionality, control output
//      4. Generic "each" support for other collections
//      5. hooks (validation, arg conversion, etc)
//      6. sub-builders?
//  2. there's no escape hatch for a failed resolution (i.e. std::option::Option),
//     and no obvious way to modify the code to support common synonyms
//  3. there's lots of paths through here that produce poor feedback. Examples:
//      1. non-struct #[derive(builder)] or tuple structs (though these ones are ~easy to fix, once enumerated)
//      2. duplicated #[builder(...)] attributes
//      3. #[builder(each = "...")] on a field whose element type fails to resolve
//  4. making the builder generic (setter<T: Into<..>(..: T) or w/e)

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let ident = input.ident;
    let builder = format_ident!("{}Builder", ident);

    let in_fields = match input.data {
        Data::Struct(DataStruct {
            fields: Fields::Named(ref named),
            ..
        }) => &named.named,
        _ => unimplemented!(),
    };

    let mut fields = vec![];
    let mut initializers = vec![];
    let mut methods = vec![];
    let mut builds = vec![];

    for &Field {
        ref ident,
        ref attrs,
        ref ty,
        ..
    } in in_fields
    {
        let mut init = quote!(#ident: std::option::Option::None);
        let (mut field, mut setter, mut build) = handle_optional_fields_helper(ident, ty);

        for ref attr @ Attribute { path, .. } in attrs {
            if !path.is_ident("builder") {
                continue;
            }

            let parsed = match attr.parse_args_with(BuilderAttr::parse) {
                Ok(parsed) => parsed,
                Err(..) => {
                    return TokenStream::from(
                        // On nightly, could use Error::new(attr.path.span().join(attr.tokens.span()), ..)
                        // Probably this means: we should've just used parse_meta in the first place for this,
                        //   or maybe: the expected feedback should be changed to the whole attr span
                        Error::new_spanned(
                            attr.parse_meta().expect("actually shouldn't expect this, it'll blow up with #[builder(some other language)], which will also cause BuilderAttr::parse to fail, but if the attribute looks almost right (like in the test) it'll get the whole attribute span"),
                            r#"expected `builder(each = "...")`"#,
                        )
                        .into_compile_error(),
                    );
                }
            };

            let each = match parsed {
                BuilderAttr::Each(ident) => ident,
            };

            let elem_ty = (|| {
                let segments = match ty {
                    Type::Path(TypePath {
                        qself: None,
                        path: Path { segments, .. },
                    }) => segments,
                    _ => return None,
                };

                let args = match segments.first() {
                    Some(PathSegment {
                        arguments: PathArguments::AngleBracketed(args),
                        ..
                    }) => &args.args,
                    _ => return None,
                };

                match args.first() {
                    Some(GenericArgument::Type(ty)) => Some(ty),
                    _ => None,
                }
            })();

            if ident.as_ref().unwrap() != &each {
                methods.push(quote! {
                    pub fn #ident(mut self, #ident: #ty) -> Self {
                        self.#ident = #ident;

                        self
                    }
                });
            }

            setter = quote! {
                pub fn #each(mut self, #each: #elem_ty) -> Self {
                    self.#ident.push(#each);

                    self
                }
            };

            init = quote!(#ident: vec![]);
            field = quote!(#ident: #ty);
            build = quote!(#ident: self.#ident);
        }

        initializers.push(init);
        fields.push(field);
        methods.push(setter);
        builds.push(build);
    }

    quote! {
        pub struct #builder {
            #(#fields),*
        }

        impl #ident {
            pub fn builder() -> #builder {
                #builder {
                    #(#initializers),*
                }
            }
        }

        impl #builder {
            #(#methods)*

            pub fn build(self) -> std::result::Result<#ident, std::boxed::Box<dyn std::error::Error>> {
                std::result::Result::Ok(#ident {
                    #(#builds),*
                })
            }
        }
    }
    .into()
}

fn handle_optional_fields_helper(
    ident: &Option<Ident>,
    ty: &Type,
) -> (
    proc_macro2::TokenStream,
    proc_macro2::TokenStream,
    proc_macro2::TokenStream,
) {
    let name = ident.as_ref().unwrap().to_string();

    let (f_ty, build_value) = if let Some(opt_ty) = unpack_option_ty(ty) {
        (opt_ty, quote!(self.#ident))
    } else {
        (
            ty,
            quote!(self.#ident.ok_or_else::<std::boxed::Box<dyn std::error::Error>, _>(|| format!("missing field {}", #name).into())?),
        )
    };

    (
        quote!(#ident: std::option::Option<#f_ty>),
        quote! {
            pub fn #ident(mut self, #ident: #f_ty) -> Self {
                self.#ident = std::option::Option::Some(#ident);

                self
            }
        },
        quote!(#ident: #build_value),
    )
}

fn unpack_option_ty(ty: &Type) -> Option<&Type> {
    let segments = match ty {
        Type::Path(TypePath {
            qself: None,
            path: Path { segments, .. },
        }) => segments,
        _ => return None,
    };

    let args = match segments.first() {
        Some(PathSegment {
            ident,
            arguments: PathArguments::AngleBracketed(args),
        }) if ident == "Option" => &args.args,
        _ => return None,
    };

    match args.first() {
        Some(GenericArgument::Type(ty)) => Some(ty),
        _ => None,
    }
}
