// Copyright (c) Meta Platforms, Inc. and affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree

use proc_macro::TokenStream;
use quote::{quote, quote_spanned};
use syn::{self, spanned::Spanned, Data, DataEnum, DeriveInput, Fields, FieldsUnnamed};

pub(crate) fn impl_codegen(ast: &DeriveInput) -> TokenStream {
    match &ast.data {
        Data::Enum(e) => impl_enum(ast, e),
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

fn impl_enum(ast: &DeriveInput, e: &DataEnum) -> TokenStream {
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
    let gen = quote! {
        impl<'a> Codegen<'a> for #ident #generics {
            fn codegen(&self, state: &mut CodegenState<'a>) {
                match self {
                    #(Self::#varnames(x) => x.codegen(state),)*
                }
            }
        }
    };
    gen.into()
}
