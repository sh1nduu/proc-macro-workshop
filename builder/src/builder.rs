use proc_macro2::{Ident, Span, TokenStream};
use quote::{format_ident, quote, quote_spanned};
use syn::{spanned::Spanned, Data, DataStruct, DeriveInput, Error, Field, Fields, Result, Type};

/// `BuilderName` holds the identifiers for the target and the associated builder.
struct BuilderName {
    /// The identifier of the target type for which the builder is created.
    target_ident: Ident,
    /// The identifier of the builder struct associated with the target type.
    builder_ident: Ident,
}

impl BuilderName {
    /// Parses the provided `DeriveInput` to construct a `BuilderName` instance.
    fn parse(input: &DeriveInput) -> Self {
        let target_ident = input.ident.clone();
        let builder_ident = format_ident!("{}Builder", target_ident);
        Self {
            target_ident,
            builder_ident,
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

    /// Generates a `TokenStream` for the setter methods of the builder.
    fn setter_methods(&self) -> TokenStream {
        let methods = self.fields.iter().map(StructField::setter_method);
        quote! { #(#methods)* }
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

    /// Generates a `TokenStream` for a setter method of the struct.
    fn setter_method(&self) -> TokenStream {
        let name = &self.name;
        let ty = &self.ty;
        quote_spanned! {self.span=>
            pub fn #name(&mut self, #name: #ty) -> &mut Self {
                self.#name = std::option::Option::Some(#name);
                self
            }
        }
    }
}

/// Generates a `TokenStream` representing the derived builder struct for a procedural macro.
pub(crate) fn expand(input: DeriveInput) -> Result<TokenStream> {
    let builder = BuilderName::parse(&input);
    let target_ident = &builder.target_ident;
    let builder_ident = &builder.builder_ident;
    let struct_fields = StructFields::parse(&input)?;
    let builder_fields = struct_fields.builder_fields();
    let init_fields = struct_fields.init_fields();
    let setter_methods = struct_fields.setter_methods();
    Ok(quote! {
        impl #target_ident {
            pub fn builder() -> #builder_ident {
                #builder_ident {
                    #init_fields
                }
            }
        }

        pub struct #builder_ident {
            #builder_fields
        }

        impl #builder_ident {
            #setter_methods
        }
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use quote::ToTokens;
    use std::iter::zip;
    use syn::{parse_quote, DeriveInput};

    #[test]
    fn builder_name_generates_a_builder_identifier_from_a_struct() {
        let input: DeriveInput = parse_quote! {
            pub struct Command { }
        };

        let builder_name = BuilderName::parse(&input);

        assert_eq!(builder_name.target_ident, "Command");
        assert_eq!(builder_name.builder_ident, "CommandBuilder");
    }

    #[test]
    fn struct_fields_parses_each_fields_of_a_struct() -> Result<()> {
        let input: DeriveInput = parse_quote! {
            struct Command {
                executable: String,
                args: Vec<String>,
                env: Vec<String>,
                current_dir: String,
            }
        };
        let expectations = [
            ["executable", &quote!(String).to_string()],
            ["args", &quote!(Vec<String>).to_string()],
            ["env", &quote!(Vec<String>).to_string()],
            ["current_dir", &quote!(String).to_string()],
        ];

        let fields = StructFields::parse(&input)?;

        assert_eq!(fields.fields.len(), 4);
        for (field, expectation) in zip(fields.fields, expectations) {
            assert_eq!(field.name.to_string(), expectation[0]);
            assert_eq!(field.ty.into_token_stream().to_string(), expectation[1]);
        }
        Ok(())
    }

    #[test]
    fn struct_field_parses_a_name_and_a_type_of_a_field() -> Result<()> {
        let field: Field = parse_quote! {
            pub name: String
        };

        let struct_field = StructField::parse(&field)?;

        assert_eq!(struct_field.name.to_string(), "name");
        assert_eq!(struct_field.ty.to_token_stream().to_string(), "String");
        Ok(())
    }

    #[test]
    fn struct_field_generates_a_builder_field() -> Result<()> {
        let field: Field = parse_quote! {
            pub name: String
        };
        let expected = quote! {
            name: std::option::Option<String>,
        };
        let struct_field = StructField::parse(&field)?;

        let builder_field = struct_field.builder_field();

        assert_eq!(builder_field.to_string(), expected.to_string());
        Ok(())
    }

    #[test]
    fn struct_field_generates_a_field_with_initialization_value() -> Result<()> {
        let field: Field = parse_quote! {
            pub name: String
        };
        let expected = quote! {
            name: std::option::Option::None,
        };
        let struct_field = StructField::parse(&field)?;

        let init_field = struct_field.init_field();

        assert_eq!(init_field.to_string(), expected.to_string());
        Ok(())
    }

    #[test]
    fn struct_field_generates_a_setter_method() -> Result<()> {
        let field: Field = parse_quote! {
            pub name: String
        };
        let expected = quote! {
            pub fn name(&mut self, name: String) -> &mut Self {
                self.name = std::option::Option::Some(name);
                self
            }
        };
        let struct_field = StructField::parse(&field)?;

        let setter_method = struct_field.setter_method();

        assert_eq!(setter_method.to_string(), expected.to_string());
        Ok(())
    }
}
