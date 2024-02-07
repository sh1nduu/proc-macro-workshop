use proc_macro2::{Ident, Span, TokenStream};
use quote::{format_ident, quote, quote_spanned};
use syn::{spanned::Spanned, Data, DataStruct, DeriveInput, Error, Field, Fields, Result, Type};

struct BuilderInput {
    target_name: Ident,
    builder_name: Ident,
}

impl BuilderInput {
    fn parse(input: &DeriveInput) -> Self {
        let target_name = input.ident.clone();
        let builder_name = format_ident!("{}Builder", target_name);
        Self {
            target_name,
            builder_name,
        }
    }
}

struct StructFields {
    fields: Vec<StructField>,
}

impl StructFields {
    fn parse(input: &DeriveInput) -> Result<Self> {
        let Data::Struct(DataStruct {
            fields: Fields::Named(ref fields),
            ..
        }) = input.data
        else {
            return Err(Error::new_spanned(
                input,
                "Builder macro only accepts a struct",
            ));
        };
        fields
            .named
            .iter()
            .map(|f| StructField::parse(f))
            .collect::<Result<Vec<_>>>()
            .map(|fields| Self { fields })
    }

    fn builder_fields(&self) -> TokenStream {
        let fields = self.fields.iter().map(StructField::builder_field);
        quote! { #(#fields)* }
    }

    fn init_fields(&self) -> TokenStream {
        let fields = self.fields.iter().map(StructField::init_field);
        quote! { #(#fields)* }
    }
}

struct StructField {
    name: Ident,
    ty: Type,
    span: Span,
}

impl StructField {
    fn parse(f: &Field) -> Result<Self> {
        let Some(name) = f.ident.clone() else {
            return Err(Error::new_spanned(f, "Missing field name"));
        };
        let ty = f.ty.clone();
        let span = f.span();
        Ok(Self { name, ty, span })
    }

    fn builder_field(&self) -> TokenStream {
        let name = &self.name;
        let ty = &self.ty;
        quote_spanned! {self.span=>
            #name: std::option::Option<#ty>,
        }
    }

    fn init_field(&self) -> TokenStream {
        let name = &self.name;
        quote_spanned! {self.span=>
            #name: std::option::Option::None,
        }
    }
}

pub(crate) fn expand(input: DeriveInput) -> Result<TokenStream> {
    let builder = BuilderInput::parse(&input);
    let target_name = &builder.target_name;
    let builder_name = &builder.builder_name;
    let struct_fields = StructFields::parse(&input)?;
    let builder_fields = struct_fields.builder_fields();
    let init_fields = struct_fields.init_fields();
    Ok(quote! {
        impl #target_name {
            pub fn builder() -> #builder_name {
                #builder_name {
                    #init_fields
                }
            }
        }

        pub struct #builder_name {
            #builder_fields
        }
    })
}

#[cfg(test)]
mod expand {
    use std::iter::zip;

    use super::*;
    use quote::ToTokens;
    use syn::DeriveInput;

    macro_rules! quote_derive_input {
        ($($tt:tt)*) => {
            syn::parse2::<DeriveInput>(quote! { $($tt)* })
        };
    }

    #[test]
    fn builder_input_parses_struct_names() -> Result<()> {
        let input = quote_derive_input! {
            #[derive(Builder)]
            pub struct Command { }
        }?;

        let builder_input = BuilderInput::parse(&input);

        assert_eq!(builder_input.target_name, "Command");
        assert_eq!(builder_input.builder_name, "CommandBuilder");

        Ok(())
    }

    #[test]
    fn struct_fields_parses_each_fields() -> Result<()> {
        let input = quote_derive_input! {
            struct Command {
                executable: String,
                args: Vec<String>,
                env: Vec<String>,
                current_dir: String,
            }
        }?;
        let expectations = [
            ["executable", "String"],
            ["args", "Vec < String >"],
            ["env", "Vec < String >"],
            ["current_dir", "String"],
        ];

        let fields = StructFields::parse(&input)?;

        assert_eq!(fields.fields.len(), 4);
        for (field, expectation) in zip(fields.fields, expectations) {
            assert_eq!(field.name.to_string(), expectation[0]);
            assert_eq!(field.ty.into_token_stream().to_string(), expectation[1]);
        }

        Ok(())
    }
}
