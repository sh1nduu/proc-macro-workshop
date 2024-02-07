use proc_macro2::TokenStream;
use quote::quote;
use syn::{DeriveInput, Result};

pub(crate) fn expand(_input: DeriveInput) -> Result<TokenStream> {
    Ok(quote! {})
}
