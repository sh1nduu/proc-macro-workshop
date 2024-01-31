use proc_macro::TokenStream;
use proc_macro2::{Ident, Span};
use quote::{quote, quote_spanned};
use syn::{parse_macro_input, spanned::Spanned, Data, DeriveInput, Field, Fields};

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = input.ident;
    let builder_name = Ident::new(&format!("{}Builder", name), Span::call_site());
    let builder_fields = get_struct_fields(&input.data).map(|f| {
        let name = &f.ident;
        let ty = &f.ty;
        quote_spanned! {f.span()=>
            #name: Option<#ty>
        }
    });
    let initial_fields = get_struct_fields(&input.data).map(|f| {
        let name = &f.ident;
        quote_spanned! {f.span()=>
            #name: None
        }
    });

    let expanded = quote! {
        impl #name {
            pub fn builder() -> #builder_name {
                #builder_name {
                    #(#initial_fields),*
                }
            }
        }
        struct #builder_name {
            #(#builder_fields),*
        }
    };

    TokenStream::from(expanded)
}

fn get_struct_fields(data: &Data) -> impl Iterator<Item = &Field> {
    match data {
        Data::Struct(ref data) => match data.fields {
            Fields::Named(ref fields) => fields.named.iter(),
            _ => unimplemented!(),
        },
        _ => unimplemented!(),
    }
}
