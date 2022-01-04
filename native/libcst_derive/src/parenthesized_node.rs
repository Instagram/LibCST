// Copyright (c) Meta Platforms, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree

use proc_macro::TokenStream;
use quote::{quote, quote_spanned};
use syn::{spanned::Spanned, Data, DataEnum, DeriveInput, Fields, FieldsUnnamed};

pub(crate) fn impl_parenthesized_node(ast: &DeriveInput) -> TokenStream {
    match &ast.data {
        Data::Enum(e) => impl_enum(ast, e),
        Data::Struct(_) => impl_struct(ast),
        Data::Union(u) => quote_spanned! {
            u.union_token.span() =>
            compile_error!("Union type is not supported")
        }
        .into(),
    }
}

fn impl_struct(ast: &DeriveInput) -> TokenStream {
    let ident = &ast.ident;
    let generics = &ast.generics;
    let gen = quote! {
        impl #generics ParenthesizedNode#generics for #ident #generics {
            fn lpar(&self) -> &Vec<LeftParen#generics> {
                &self.lpar
            }
            fn rpar(&self) -> &Vec<RightParen#generics> {
                &self.rpar
            }
            fn with_parens(self, left: LeftParen#generics, right: RightParen#generics) -> Self {
                let mut lpar = self.lpar;
                let mut rpar = self.rpar;
                lpar.insert(0, left);
                rpar.push(right);
                #[allow(clippy::needless_update)]
                Self { lpar, rpar, ..self }
            }
        }
    };
    gen.into()
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
        impl #generics ParenthesizedNode#generics for #ident #generics {
            fn lpar(&self) -> &Vec<LeftParen #generics > {
                match self {
                    #(Self::#varnames(x) => x.lpar(),)*
                }
            }
            fn rpar(&self) -> &Vec<RightParen #generics > {
                match self {
                    #(Self::#varnames(x) => x.rpar(),)*
                }
            }
            fn with_parens(self, left: LeftParen #generics, right: RightParen #generics) -> Self {
                match self {
                    #(Self::#varnames(x) => Self::#varnames(x.with_parens(left, right)),)*
                }
            }
        }
    };
    gen.into()
}
