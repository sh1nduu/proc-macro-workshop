use proc_macro2::{Ident, Span, TokenStream};
use quote::{format_ident, quote, quote_spanned};
use syn::{spanned::Spanned, Data, DataStruct, DeriveInput, Error, Field, Fields, Result, Type};

/// `BuilderName` holds the identifiers for the target and the associated builder.
struct BuilderName {
    /// The identifier of the target type for which the builder is created.
    target_name: Ident,
    /// The identifier of the builder structure associated with the target type.
    builder_name: Ident,
}

impl BuilderName {
    /// Parses the provided `DeriveInput` to construct a `BuilderName` instance.
    fn parse(input: &DeriveInput) -> Self {
        let target_name = input.ident.clone();
        let builder_name = format_ident!("{}Builder", target_name);
        Self {
            target_name,
            builder_name,
        }
    }
}

/// `StructFields` holds the fields of the struct.
struct StructFields {
    fields: Vec<StructField>,
}

impl StructFields {
    /// Parses the provided `DeriveInput` to construct a `StructFields` instance.
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

    /// Generates a `TokenStream` of the builder field declarations.
    fn builder_fields(&self) -> TokenStream {
        let fields = self.fields.iter().map(StructField::builder_field);
        quote! { #(#fields)* }
    }

    /// Generates a `TokenStream` of the builder fields initialized with default values, typically `None`.
    fn init_fields(&self) -> TokenStream {
        let fields = self.fields.iter().map(StructField::init_field);
        quote! { #(#fields)* }
    }
}

/// `StructField` holds metadata for a field of the provided struct.
struct StructField {
    /// The name of the field.
    name: Ident,
    /// The type of the field.
    ty: Type,
    /// The location of the field in the source code.
    span: Span,
}

impl StructField {
    /// Parses the provided `syn::Field` to construct a `StructField` instance.
    ///
    /// This function takes a reference to a `syn::Field`, and construct a `StructField` containing metadata.
    /// If the field does not have a name, it returns an error.
    fn parse(f: &Field) -> Result<Self> {
        let Some(name) = f.ident.clone() else {
            return Err(Error::new_spanned(f, "Missing field name"));
        };
        let ty = f.ty.clone();
        let span = f.span();
        Ok(Self { name, ty, span })
    }

    /// Generates a `TokenStream` of the declaration for the builder struct field.
    fn builder_field(&self) -> TokenStream {
        let name = &self.name;
        let ty = &self.ty;
        quote_spanned! {self.span=>
            #name: std::option::Option<#ty>,
        }
    }

    /// Generates a `TokenStream` of a struct field with a default value, typically `None`.
    fn init_field(&self) -> TokenStream {
        let name = &self.name;
        quote_spanned! {self.span=>
            #name: std::option::Option::None,
        }
    }
}

/// Generates a `TokenStream` representing the derived builder struct for a procedural macro.
pub(crate) fn expand(input: DeriveInput) -> Result<TokenStream> {
    let builder = BuilderName::parse(&input);
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
    use super::*;
    use quote::ToTokens;
    use std::iter::zip;
    use syn::{parse_quote, DeriveInput};

    #[test]
    fn builder_input_parses_struct_names() {
        let input: DeriveInput = parse_quote! {
            pub struct Command { }
        };

        let builder_input = BuilderName::parse(&input);

        assert_eq!(builder_input.target_name, "Command");
        assert_eq!(builder_input.builder_name, "CommandBuilder");
    }

    #[test]
    fn struct_fields_parses_each_fields() -> Result<()> {
        let input: DeriveInput = parse_quote! {
            struct Command {
                executable: String,
                args: Vec<String>,
                env: Vec<String>,
                current_dir: String,
            }
        };
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
