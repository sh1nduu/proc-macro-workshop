use proc_macro2::{Ident, Span, TokenStream};
use quote::{format_ident, quote, quote_spanned, ToTokens};
use syn::{
    spanned::Spanned, AngleBracketedGenericArguments, Attribute, Data, DataStruct, DeriveInput,
    Error, Field, Fields, GenericArgument, LitStr, Path, PathArguments, PathSegment, Result, Type,
    TypePath,
};

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

    /// Generates a `TokenStream` for the setter methods of the builder.
    fn setter_methods(&self) -> TokenStream {
        let methods = self.fields.iter().map(StructField::setter_method);
        quote! { #(#methods)* }
    }
}

/// `FieldType` holds metadata for some specific type, e.g. `Option<T>`.
#[derive(PartialEq)]
enum FieldType {
    Normal(Type),
    Option(Type, Type),
    Vec(Type, Type),
    VecEach(Type, Type, Ident),
}

impl FieldType {
    fn parse(ty: &Type, attrs: &[Attribute]) -> Result<Self> {
        let option_inner = get_inner_type("Option", ty);
        if option_inner.is_some() {
            return Ok(Self::Option(ty.clone(), option_inner.unwrap()));
        }
        let vec_inner = get_inner_type("Vec", ty);
        if vec_inner.is_some() {
            if let Some(ident) = parse_each_ident(&attrs)? {
                return Ok(Self::VecEach(ty.clone(), vec_inner.unwrap(), ident));
            }
            return Ok(Self::Vec(ty.clone(), vec_inner.unwrap()));
        }
        Ok(Self::Normal(ty.clone()))
    }
}

fn get_inner_type(wrapper: &str, ty: &Type) -> Option<Type> {
    match ty {
        Type::Path(TypePath {
            path: Path { segments, .. },
            ..
        }) => match segments.first() {
            Some(PathSegment {
                ident,
                arguments:
                    PathArguments::AngleBracketed(AngleBracketedGenericArguments { args, .. }),
                ..
            }) if ident == wrapper => match args.first() {
                Some(GenericArgument::Type(inner)) => Some(inner.clone()),
                _ => None,
            },
            _ => None,
        },
        _ => None,
    }
}

fn parse_each_ident(attrs: &[Attribute]) -> Result<Option<Ident>> {
    match attrs.first() {
        Some(attr) if attr.path().is_ident("builder") => {
            let mut each_ident = None;
            let _ = attr.parse_nested_meta(|meta| {
                if !meta.path.is_ident("each") {
                    return Err(Error::new_spanned(
                        attr.meta.clone(),
                        r#"expected `builder(each = "...")`"#,
                    ));
                }
                each_ident = meta
                    .value()
                    .and_then(|v| v.parse::<LitStr>())
                    .map(|lit| {
                        let span = lit.span();
                        let name = lit.to_token_stream().to_string().replace('"', "");
                        Ident::new(&name, span)
                    })
                    .ok();
                Ok(())
            })?;
            Ok(each_ident)
        }
        _ => Ok(None),
    }
}

impl ToTokens for FieldType {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Self::Normal(ty) => ty.to_tokens(tokens),
            Self::Option(ty, _) => ty.to_tokens(tokens),
            Self::Vec(ty, _) => ty.to_tokens(tokens),
            Self::VecEach(ty, _, _) => ty.to_tokens(tokens),
        };
    }
}

/// `StructField` holds metadata for a field of the provided struct.
struct StructField {
    /// The name of the field.
    name: Ident,
    /// The type of the field.
    ty: FieldType,
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
        let ty = FieldType::parse(&f.ty, &f.attrs)?;
        let span = f.span();
        Ok(Self { name, ty, span })
    }

    /// Generates a `TokenStream` of the declaration for the builder struct field.
    fn builder_field(&self) -> TokenStream {
        let name = &self.name;
        match &self.ty {
            FieldType::Normal(ty) | FieldType::Option(_, ty) => quote_spanned! {self.span=>
                #name: std::option::Option<#ty>,
            },
            FieldType::Vec(_, inner) | FieldType::VecEach(_, inner, _) => {
                quote_spanned! {self.span=>
                    #name: std::vec::Vec<#inner>,
                }
            }
        }
    }

    /// Generates a `TokenStream` for a setter method of the struct.
    fn setter_method(&self) -> TokenStream {
        let name = &self.name;
        match &self.ty {
            FieldType::Normal(ty) => quote_spanned! {self.span=>
                pub fn #name(&mut self, #name: #ty) -> &mut Self {
                    self.#name = std::option::Option::Some(#name);
                    self
                }
            },
            FieldType::Option(_, inner) => quote_spanned! {self.span=>
                pub fn #name(&mut self, #name: #inner) -> &mut Self {
                    self.#name = std::option::Option::Some(#name);
                    self
                }
            },
            FieldType::Vec(_, inner) => quote_spanned! {self.span=>
                pub fn #name(&mut self, #name: std::vec::Vec<#inner>) -> &mut Self {
                    self.#name = #name;
                    self
                }
            },
            FieldType::VecEach(_, inner, each) => quote_spanned! {self.span=>
                pub fn #each(&mut self, #each: #inner) -> &mut Self {
                    self.#name.push(#each);
                    self
                }
            },
        }
    }
}

/// Generates a `TokenStream` representing the derived builder method for the given struct.
fn expand_builder_method(builder_name: &BuilderName, struct_fields: &StructFields) -> TokenStream {
    let init_fields = struct_fields.fields.iter().map(|f| {
        let name = &f.name;
        match &f.ty {
            FieldType::Normal(_) | FieldType::Option(_, _) => quote_spanned! {f.span=>
                #name: std::option::Option::None,
            },
            FieldType::Vec(_, _) | FieldType::VecEach(_, _, _) => quote_spanned! {f.span=>
                #name: std::vec::Vec::new(),
            },
        }
    });
    let builder_ident = &builder_name.builder_ident;

    quote! {
        pub fn builder() -> #builder_ident {
            #builder_ident {
                #(#init_fields)*
            }
        }
    }
}

/// Generates a `TokenStream` representing the derived build method for the builder struct.
fn expand_build_method(builder_name: &BuilderName, struct_fields: &StructFields) -> TokenStream {
    let prepares = struct_fields.fields.iter().map(|f| {
        let name = &f.name;
        match &f.ty {
            FieldType::Normal(_) => quote_spanned! {f.span=>
                let std::option::Option::Some(ref #name) = self.#name else {
                    return std::result::Result::Err("Insufficient field".to_string().into());
                };
            },
            FieldType::Option(_, _) | FieldType::Vec(_, _) | FieldType::VecEach(_, _, _) => {
                quote_spanned! {f.span=>
                    let #name = &self.#name;
                }
            }
        }
    });
    let fields = struct_fields.fields.iter().map(|f| {
        let name = &f.name;
        quote_spanned! {f.span=>
            #name: #name.clone()
        }
    });
    let target_ident = &builder_name.target_ident;

    quote! {
        pub fn build(&mut self) -> std::result::Result<#target_ident, std::boxed::Box<dyn std::error::Error>> {
            #(#prepares)*

            std::result::Result::Ok(#target_ident {
                #(#fields),*
            })
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
    let setter_methods = struct_fields.setter_methods();
    let builder_method = expand_builder_method(&builder, &struct_fields);
    let build_method = expand_build_method(&builder, &struct_fields);
    Ok(quote! {
        impl #target_ident {
            #builder_method
        }

        pub struct #builder_ident {
            #builder_fields
        }

        impl #builder_ident {
            #setter_methods
            #build_method
        }
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use quote::ToTokens;
    use std::iter::zip;
    use syn::{parse_quote, DeriveInput};

    mod builder_name {
        use super::*;

        #[test]
        fn generates_a_builder_identifier_from_a_struct() {
            let input: DeriveInput = parse_quote! {
                pub struct Command { }
            };

            let builder_name = BuilderName::parse(&input);

            assert_eq!(builder_name.target_ident, "Command");
            assert_eq!(builder_name.builder_ident, "CommandBuilder");
        }
    }

    mod struct_fields {
        use super::*;

        #[test]
        fn parses_each_fields_of_a_struct() -> Result<()> {
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
    }

    mod expand_builder_method {
        use super::*;

        #[test]
        fn generates_a_builder_method() -> Result<()> {
            let input: DeriveInput = parse_quote! {
                struct Command {
                    executable: String,
                    #[builder(each = "arg")]
                    args: Vec<String>,
                    env: Vec<String>,
                    current_dir: Option<String>
                }
            };
            let builder_name = BuilderName::parse(&input);
            let fields = StructFields::parse(&input)?;

            let builder_method = expand_builder_method(&builder_name, &fields);

            let expected = quote! {
                pub fn builder() -> CommandBuilder {
                    CommandBuilder {
                        executable: std::option::Option::None,
                        args: std::vec::Vec::new(),
                        env: std::vec::Vec::new(),
                        current_dir: std::option::Option::None,
                    }
                }
            };
            assert_eq!(builder_method.to_string(), expected.to_string());
            Ok(())
        }
    }

    mod expand_build_method {
        use super::*;

        #[test]
        fn generates_a_build_method() -> Result<()> {
            let input: DeriveInput = parse_quote! {
                struct Command {
                    executable: String,
                    #[builder(each = "arg")]
                    args: Vec<String>,
                    env: Vec<String>,
                    current_dir: Option<String>,
                }
            };
            let builder_name = BuilderName::parse(&input);
            let fields = StructFields::parse(&input)?;

            let build_method = expand_build_method(&builder_name, &fields);

            let expected = quote! {
                pub fn build(&mut self) -> std::result::Result<Command, std::boxed::Box<dyn std::error::Error>> {
                    let std::option::Option::Some(ref executable) = self.executable else {
                        return std::result::Result::Err("Insufficient field".to_string().into());
                    };
                    let args = &self.args;
                    let env = &self.env;
                    let current_dir = &self.current_dir;
                    std::result::Result::Ok(Command {
                        executable: executable.clone(),
                        args: args.clone(),
                        env: env.clone(),
                        current_dir: current_dir.clone()
                    })
                }
            };
            assert_eq!(build_method.to_string(), expected.to_string());
            Ok(())
        }
    }

    mod struct_field {
        use super::*;

        #[test]
        fn parses_a_name_and_a_type_of_a_field() -> Result<()> {
            let field: Field = parse_quote! {
                pub name: String
            };

            let struct_field = StructField::parse(&field)?;

            assert_eq!(struct_field.name.to_string(), "name");
            assert_eq!(
                struct_field.ty.to_token_stream().to_string(),
                quote!(String).to_string()
            );
            assert!(matches!(struct_field.ty, FieldType::Normal(_)));
            Ok(())
        }

        #[test]
        fn parses_an_option_type() -> Result<()> {
            let field: Field = parse_quote! {
                pub name: Option<String>
            };

            let struct_field = StructField::parse(&field)?;

            assert_eq!(struct_field.name.to_string(), "name");
            assert_eq!(
                struct_field.ty.to_token_stream().to_string(),
                quote!(Option<String>).to_string()
            );
            assert!(matches!(struct_field.ty, FieldType::Option(_, _)));

            Ok(())
        }

        #[test]
        fn parses_a_vector_field() -> Result<()> {
            let field: Field = parse_quote! {
                pub names: Vec<String>
            };

            let struct_field = StructField::parse(&field)?;
            assert_eq!(struct_field.name.to_string(), "names");
            assert_eq!(
                struct_field.ty.to_token_stream().to_string(),
                quote!(Vec<String>).to_string()
            );
            assert!(matches!(struct_field.ty, FieldType::Vec(_, _)));

            Ok(())
        }

        #[test]
        fn parses_a_vector_field_with_a_each_attribute() -> Result<()> {
            let field: Field = parse_quote! {
                #[builder(each = "name")]
                pub names: Vec<String>
            };

            let struct_field = StructField::parse(&field)?;
            assert_eq!(struct_field.name.to_string(), "names");
            assert_eq!(
                struct_field.ty.to_token_stream().to_string(),
                quote!(Vec<String>).to_string()
            );
            assert!(matches!(struct_field.ty, FieldType::VecEach(_, _, _)));

            Ok(())
        }

        #[test]
        fn generates_a_optional_builder_field() -> Result<()> {
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
        fn generates_a_optional_builder_field_when_option_type_is_given() -> Result<()> {
            let field: Field = parse_quote! {
                pub name: Option<String>
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
        fn generates_a_vector_builder_field_when_vec_is_given() -> Result<()> {
            let field: Field = parse_quote! {
                pub names: Vec<String>
            };
            let expected = quote! {
                names: std::vec::Vec<String>,
            };
            let struct_field = StructField::parse(&field)?;

            let builder_field = struct_field.builder_field();

            assert_eq!(builder_field.to_string(), expected.to_string());
            Ok(())
        }

        #[test]
        fn generates_a_vector_builder_field_with_each_name() -> Result<()> {
            let field: Field = parse_quote! {
                #[builder(each = "name")]
                pub names: Vec<String>
            };
            let expected = quote! {
                names: std::vec::Vec<String>,
            };
            let struct_field = StructField::parse(&field)?;

            let builder_field = struct_field.builder_field();

            assert_eq!(builder_field.to_string(), expected.to_string());
            Ok(())
        }

        #[test]
        fn generates_a_setter() -> Result<()> {
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

        #[test]
        fn generates_a_setter_which_takes_a_inner_type_of_the_option_field() -> Result<()> {
            let field: Field = parse_quote! {
                pub name: Option<String>
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

        #[test]
        fn generates_a_setter_which_takes_vec_type_for_vec_field() -> Result<()> {
            let field: Field = parse_quote! {
                pub names: Vec<String>
            };
            let expected = quote! {
                pub fn names(&mut self, names: std::vec::Vec<String>) -> &mut Self {
                    self.names = names;
                    self
                }
            };
            let struct_field = StructField::parse(&field)?;

            let setter_method = struct_field.setter_method();

            assert_eq!(setter_method.to_string(), expected.to_string());
            Ok(())
        }

        #[test]
        fn generates_a_setter_which_takes_a_inner_type_of_the_vec_field() -> Result<()> {
            let field: Field = parse_quote! {
                #[builder(each = "name")]
                pub names: Vec<String>
            };
            let expected = quote! {
                pub fn name(&mut self, name: String) -> &mut Self {
                    self.names.push(name);
                    self
                }
            };
            let struct_field = StructField::parse(&field)?;

            let setter_method = struct_field.setter_method();

            assert_eq!(setter_method.to_string(), expected.to_string());
            Ok(())
        }
    }
}
