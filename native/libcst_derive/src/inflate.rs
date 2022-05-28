// Copyright (c) Meta Platforms, Inc. and affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree

use proc_macro::TokenStream;
use quote::{format_ident, quote, quote_spanned};
use syn::{self, spanned::Spanned, Data, DataEnum, DeriveInput, Fields, FieldsUnnamed};

pub(crate) fn impl_inflate(ast: &DeriveInput) -> TokenStream {
    match &ast.data {
        Data::Enum(e) => impl_inflate_enum(ast, e),
        Data::Struct(s) => quote_spanned! {
            s.struct_token.span() =>
            compile_error!("Struct type is not supported")
        }
        .into(),
        Data::Union(u) => quote_spanned! {
            u.union_token.span() =>
            compile_error!("Union type is not supported")
        }
        .into(),
    }
}

fn impl_inflate_enum(ast: &DeriveInput, e: &DataEnum) -> TokenStream {
    let mut varnames = vec![];
    for var in e.variants.iter() {
        match &var.fields {
            Fields::Named(n) => {
                return quote_spanned! {
                    n.span() =>
                    compile_error!("Named enum fields not supported")
                }
                .into()
            }
            f @ Fields::Unit => {
                return quote_spanned! {
                    f.span() =>
                    compile_error!("Empty enum variants not supported")
                }
                .into()
            }
            Fields::Unnamed(FieldsUnnamed { unnamed, .. }) => {
                if unnamed.len() > 1 {
                    return quote_spanned! {
                        unnamed.span() =>
                        compile_error!("Multiple unnamed fields not supported")
                    }
                    .into();
                }
                varnames.push(&var.ident);
            }
        }
    }
    let ident = &ast.ident;
    let generics = &ast.generics;
    let ident_str = ident.to_string();
    let inflated_ident = format_ident!(
        "{}",
        ident_str
            .strip_prefix("Deflated")
            .expect("Cannot implement Inflate on a non-Deflated item")
    );
    let gen = quote! {
        impl#generics Inflate<'a> for #ident #generics {
            type Inflated = #inflated_ident <'a>;
            fn inflate(mut self, config: & crate::tokenizer::whitespace_parser::Config<'a>) -> std::result::Result<Self::Inflated, crate::tokenizer::whitespace_parser::WhitespaceError> {
                match self {
                    #(Self::#varnames(x) => Ok(Self::Inflated::#varnames(x.inflate(config)?)),)*
                }
            }
        }
    };
    gen.into()
}
