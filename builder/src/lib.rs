use proc_macro::TokenStream;
use proc_macro2::{Ident, Span};
use quote::{quote, quote_spanned, ToTokens};
use syn::{
    parse_macro_input, spanned::Spanned, Data, DeriveInput, Field, Fields, GenericArgument, LitStr,
    PathArguments, Type, TypePath,
};

#[derive(Debug)]
enum BuilderField {
    Normal(NormalField),
    Option(OptionField),
    Vec(VectorField),
}

trait Compile {
    fn name(&self) -> proc_macro2::TokenStream;
    fn field_type(&self) -> proc_macro2::TokenStream;
    fn arg_type(&self) -> proc_macro2::TokenStream;
    fn arrange(&self) -> proc_macro2::TokenStream;
    fn default(&self) -> proc_macro2::TokenStream;
    fn assignment(&self) -> proc_macro2::TokenStream;
    fn builder_impl(&self) -> proc_macro2::TokenStream {
        self.builder_impl_default()
    }
    fn builder_impl_default(&self) -> proc_macro2::TokenStream {
        let name = self.name();
        let arg_type = self.arg_type();
        let assignment = self.assignment();
        let span = self.span();
        quote_spanned! {span=>
            fn #name(&mut self, #name: #arg_type) -> &mut Self {
                #assignment
                self
            }
        }
    }
    fn span(&self) -> Span;
}

macro_rules! call_inner {
    ($self:ident,$fn:ident) => {
        match $self {
            Self::Normal(f) => f.$fn(),
            Self::Option(f) => f.$fn(),
            Self::Vec(f) => f.$fn(),
        }
    };
}

impl BuilderField {
    fn normal(f: &Field) -> Self {
        Self::Normal(NormalField {
            field: FieldInfo::new(f),
        })
    }
    fn option(f: &Field, inner_type: &Type) -> Self {
        Self::Option(OptionField {
            field: FieldInfo::new(f),
            inner_type: inner_type.clone(),
        })
    }
    fn vec(f: &Field, each_name: &LitStr, inner_type: &Type) -> Self {
        Self::Vec(VectorField::new(f, each_name, inner_type))
    }
    fn to_builder_field(&self) -> proc_macro2::TokenStream {
        let name = call_inner!(self, name);
        let field_type = call_inner!(self, field_type);
        let span = call_inner!(self, span);
        quote_spanned! {span => #name: #field_type }
    }
    fn to_builder_impl(&self) -> proc_macro2::TokenStream {
        call_inner!(self, builder_impl)
    }
    fn to_builder_build_arrage(&self) -> proc_macro2::TokenStream {
        let arrange = call_inner!(self, arrange);
        let span = call_inner!(self, span);
        quote_spanned! {span=> #arrange }
    }
    fn to_builder_build_names(&self) -> proc_macro2::TokenStream {
        let name = call_inner!(self, name);
        let span = call_inner!(self, span);
        quote_spanned! {span=> #name: #name.clone() }
    }
    fn to_builder_default(&self) -> proc_macro2::TokenStream {
        let name = call_inner!(self, name);
        let default = call_inner!(self, default);
        let span = call_inner!(self, span);
        quote_spanned! {span=>
            #name: #default
        }
    }
}

#[derive(Debug)]
struct FieldInfo {
    name: Ident,
    ty: Type,
    span: Span,
}

impl FieldInfo {
    fn new(f: &Field) -> Self {
        Self {
            name: f.ident.clone().unwrap(),
            ty: f.ty.clone(),
            span: f.span(),
        }
    }
}

#[derive(Debug)]
struct NormalField {
    field: FieldInfo,
}

impl Compile for NormalField {
    fn name(&self) -> proc_macro2::TokenStream {
        let name = &self.field.name;
        quote! { #name }
    }

    fn field_type(&self) -> proc_macro2::TokenStream {
        let ty = &self.field.ty;
        quote! { Option<#ty> }
    }

    fn arg_type(&self) -> proc_macro2::TokenStream {
        let ty = &self.field.ty;
        quote! { #ty }
    }

    fn arrange(&self) -> proc_macro2::TokenStream {
        let name = self.name();
        let error_msg = format!("Insufficient field: {}", name.to_string());
        quote! {
            let Some(ref #name) = self.#name else { return Err(#error_msg.into()) };
        }
    }

    fn default(&self) -> proc_macro2::TokenStream {
        quote! { None }
    }

    fn assignment(&self) -> proc_macro2::TokenStream {
        let name = self.name();
        quote_spanned! {self.span()=>
            self.#name = Some(#name);
        }
    }

    fn span(&self) -> Span {
        self.field.span.clone()
    }
}

#[derive(Debug)]
struct OptionField {
    field: FieldInfo,
    inner_type: Type,
}

impl Compile for OptionField {
    fn name(&self) -> proc_macro2::TokenStream {
        let name = &self.field.name;
        quote! { #name }
    }

    fn field_type(&self) -> proc_macro2::TokenStream {
        let ty = &self.field.ty;
        quote! { #ty }
    }

    fn arg_type(&self) -> proc_macro2::TokenStream {
        let inner_type = &self.inner_type;
        quote! { #inner_type }
    }

    fn arrange(&self) -> proc_macro2::TokenStream {
        let name = self.name();
        quote! {
            let #name = self.#name.clone();
        }
    }

    fn default(&self) -> proc_macro2::TokenStream {
        quote! { None }
    }

    fn assignment(&self) -> proc_macro2::TokenStream {
        let name = self.name();
        quote_spanned! {self.span()=>
            self.#name = Some(#name);
        }
    }

    fn span(&self) -> Span {
        self.field.span.clone()
    }
}

#[derive(Debug)]
struct VectorField {
    field: FieldInfo,
    each_name: Ident,
    inner_type: Type,
}

impl VectorField {
    fn new(f: &Field, each_name: &LitStr, inner_type: &Type) -> Self {
        let each_name_span = each_name.span();
        let each_name = each_name.to_token_stream().to_string().replace('"', "");
        let each_name_id = Ident::new(&each_name, each_name_span);
        Self {
            field: FieldInfo::new(f),
            each_name: each_name_id,
            inner_type: inner_type.clone(),
        }
    }

    fn has_same_name(&self) -> bool {
        self.field.name == self.each_name
    }

    fn builder_impl_each(&self) -> proc_macro2::TokenStream {
        let inner_type = &self.inner_type;
        let name = &self.field.name;
        let each_name = &self.each_name;
        quote_spanned! {self.span()=>
            fn #each_name(&mut self, #each_name: #inner_type) -> &mut Self {
                self.#name.push(#each_name);
                self
            }
        }
    }
}

impl Compile for VectorField {
    fn name(&self) -> proc_macro2::TokenStream {
        let name = &self.field.name;
        quote! { #name }
    }

    fn field_type(&self) -> proc_macro2::TokenStream {
        let ty = &self.field.ty;
        quote! { #ty }
    }

    fn arg_type(&self) -> proc_macro2::TokenStream {
        let ty = &self.field.ty;
        quote! { #ty }
    }

    fn arrange(&self) -> proc_macro2::TokenStream {
        let name = self.name();
        quote! {
            let #name = self.#name.clone();
        }
    }

    fn default(&self) -> proc_macro2::TokenStream {
        quote! { vec![] }
    }

    fn assignment(&self) -> proc_macro2::TokenStream {
        let name = self.name();
        quote_spanned! {self.span()=>
            self.#name = #name;
        }
    }

    fn builder_impl(&self) -> proc_macro2::TokenStream {
        let each_impl = self.builder_impl_each();
        let default = self.builder_impl_default();

        if self.has_same_name() {
            each_impl
        } else {
            quote! {
                #default
                #each_impl
            }
        }
    }

    fn span(&self) -> Span {
        self.field.span.clone()
    }
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = input.ident;
    let builder_name = Ident::new(&format!("{}Builder", name), Span::call_site());
    let fields = get_builder_fields(&input.data);

    let builder_fields = fields.iter().map(BuilderField::to_builder_field);
    let builder_impls = fields.iter().map(BuilderField::to_builder_impl);
    let builder_build_arrange_fields = fields.iter().map(BuilderField::to_builder_build_arrage);
    let builder_build_field_names = fields.iter().map(BuilderField::to_builder_build_names);
    let initial_fields = fields.iter().map(BuilderField::to_builder_default);

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
        impl #builder_name {
            #(#builder_impls)*

            pub fn build(&mut self) -> Result<#name, Box<dyn std::error::Error>> {
                #(#builder_build_arrange_fields)*

                Ok(#name {
                    #(#builder_build_field_names),*
                })
            }
        }
    };

    TokenStream::from(expanded)
}

fn get_builder_fields(data: &Data) -> Vec<BuilderField> {
    get_struct_fields(data)
        .map(|f| {
            let ty = &f.ty;
            let option = get_option_inner_type(ty);
            let each = f
                .attrs
                .iter()
                .filter(|attr| attr.path().is_ident("builder"))
                .filter_map(|attr| {
                    let mut each = None;
                    let _ = attr.parse_nested_meta(|meta| {
                        if meta.path.is_ident("each") {
                            let value = meta.value();
                            let s: Option<LitStr> = value.and_then(|v| v.parse()).ok();
                            each = s;
                            return Ok(());
                        }
                        Err(meta.error("invalid"))
                    });
                    each
                })
                .collect::<Vec<_>>()
                .first()
                .map(|s| s.clone());

            if option.is_some() {
                BuilderField::option(f, &option.unwrap())
            } else if each.is_some() {
                let inner = get_inner_type(ty).unwrap();
                BuilderField::vec(f, &each.clone().unwrap().clone(), &inner)
            } else {
                BuilderField::normal(f)
            }
        })
        .collect::<Vec<_>>()
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

fn get_option_inner_type(ty: &Type) -> Option<Type> {
    let Type::Path(TypePath { path, .. }) = ty else {
        return None;
    };
    let Some(segment) = path.segments.first() else {
        return None;
    };
    if segment.ident.to_string() != "Option".to_string() {
        return None;
    }
    let PathArguments::AngleBracketed(ref arguments) = segment.arguments else {
        return None;
    };
    let Some(GenericArgument::Type(generic_type)) = arguments.args.first() else {
        return None;
    };
    Some(generic_type.clone())
}

fn get_inner_type(ty: &Type) -> Option<Type> {
    let Type::Path(TypePath { path, .. }) = ty else {
        return None;
    };
    let Some(segment) = path.segments.first() else {
        return None;
    };
    let PathArguments::AngleBracketed(ref arguments) = segment.arguments else {
        return None;
    };
    let Some(GenericArgument::Type(generic_type)) = arguments.args.first() else {
        return None;
    };
    Some(generic_type.clone())
}
