// Copyright (c) Meta Platforms, Inc. and affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree

mod inflate;
use inflate::impl_inflate;
mod parenthesized_node;
use parenthesized_node::impl_parenthesized_node;
mod codegen;
use codegen::impl_codegen;
mod into_py;
use into_py::impl_into_py;
mod cstnode;
use cstnode::{impl_cst_node, CSTNodeParams};

use proc_macro::TokenStream;
use syn::{parse_macro_input, DeriveInput};

#[proc_macro_derive(Inflate)]
pub fn inflate_derive(input: TokenStream) -> TokenStream {
    let ast = syn::parse(input).unwrap();
    impl_inflate(&ast)
}

#[proc_macro_derive(ParenthesizedNode)]
pub fn parenthesized_node_derive(input: TokenStream) -> TokenStream {
    impl_parenthesized_node(&syn::parse(input).unwrap(), false)
}

#[proc_macro_derive(ParenthesizedDeflatedNode)]
pub fn parenthesized_deflated_node_derive(input: TokenStream) -> TokenStream {
    impl_parenthesized_node(&syn::parse(input).unwrap(), true)
}

#[proc_macro_derive(Codegen)]
pub fn codegen_derive(input: TokenStream) -> TokenStream {
    impl_codegen(&syn::parse(input).unwrap())
}

#[proc_macro_derive(TryIntoPy, attributes(skip_py, no_py_default))]
pub fn into_py(input: TokenStream) -> TokenStream {
    impl_into_py(&syn::parse(input).unwrap())
}

#[proc_macro_attribute]
pub fn cst_node(args: TokenStream, input: TokenStream) -> TokenStream {
    let args = parse_macro_input!(args as CSTNodeParams);
    impl_cst_node(parse_macro_input!(input as DeriveInput), args)
}
