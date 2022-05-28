// Copyright (c) Meta Platforms, Inc. and affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree

use proc_macro::TokenStream;
use quote::{quote, quote_spanned};
use syn::{
    parse_quote, spanned::Spanned, Data, DataEnum, DeriveInput, Fields, FieldsUnnamed, Ident,
};

pub(crate) fn impl_parenthesized_node(ast: &DeriveInput, deflated: bool) -> TokenStream {
    match &ast.data {
        Data::Enum(e) => impl_enum(ast, e, deflated),
        Data::Struct(_) => impl_struct(ast, deflated),
        Data::Union(u) => quote_spanned! {
            u.union_token.span() =>
            compile_error!("Union type is not supported")
        }
        .into(),
    }
}

fn idents(deflated: bool) -> (Ident, Ident, Ident) {
    let treyt: Ident = if deflated {
        parse_quote!(ParenthesizedDeflatedNode)
    } else {
        parse_quote!(ParenthesizedNode)
    };
    let leftparen: Ident = if deflated {
        parse_quote!(DeflatedLeftParen)
    } else {
        parse_quote!(LeftParen)
    };
    let rightparen: Ident = if deflated {
        parse_quote!(DeflatedRightParen)
    } else {
        parse_quote!(RightParen)
    };
    (treyt, leftparen, rightparen)
}

fn impl_struct(ast: &DeriveInput, deflated: bool) -> TokenStream {
    let ident = &ast.ident;
    let generics = if deflated {
        parse_quote!(<'r, 'a>)
    } else {
        ast.generics.clone()
    };

    let (treyt, leftparen, rightparen) = idents(deflated);
    let gen = quote! {
        impl#generics #treyt#generics for #ident #generics {
            fn lpar(&self) -> &Vec<#leftparen#generics> {
                &self.lpar
            }
            fn rpar(&self) -> &Vec<#rightparen#generics> {
                &self.rpar
            }
            fn with_parens(self, left: #leftparen#generics, right: #rightparen#generics) -> Self {
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

fn impl_enum(ast: &DeriveInput, e: &DataEnum, deflated: bool) -> TokenStream {
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
    let generics = if deflated {
        parse_quote!(<'r, 'a>)
    } else {
        ast.generics.clone()
    };
    let (treyt, leftparen, rightparen) = idents(deflated);
    let gen = quote! {
        impl#generics #treyt#generics for #ident #generics {
            fn lpar(&self) -> &Vec<#leftparen#generics> {
                match self {
                    #(Self::#varnames(x) => x.lpar(),)*
                }
            }
            fn rpar(&self) -> &Vec<#rightparen#generics> {
                match self {
                    #(Self::#varnames(x) => x.rpar(),)*
                }
            }
            fn with_parens(self, left: #leftparen#generics, right: #rightparen#generics) -> Self {
                match self {
                    #(Self::#varnames(x) => Self::#varnames(x.with_parens(left, right)),)*
                }
            }
        }
    };
    gen.into()
}
