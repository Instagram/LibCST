// Copyright (c) Meta Platforms, Inc. and affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree

use proc_macro::TokenStream;
use quote::{format_ident, quote, quote_spanned, ToTokens};
use syn::{
    spanned::Spanned, Attribute, Data, DataEnum, DataStruct, DeriveInput, Fields, FieldsNamed,
    FieldsUnnamed, Type, TypePath, Visibility,
};

pub(crate) fn impl_into_py(ast: &DeriveInput) -> TokenStream {
    match &ast.data {
        Data::Enum(e) => impl_into_py_enum(ast, e),
        Data::Struct(s) => impl_into_py_struct(ast, s),
        Data::Union(u) => quote_spanned! {
            u.union_token.span() =>
            compile_error!("Union type is not supported")
        }
        .into(),
    }
}

fn impl_into_py_enum(ast: &DeriveInput, e: &DataEnum) -> TokenStream {
    let mut toks = vec![];
    for var in e.variants.iter() {
        let varname = &var.ident;
        match &var.fields {
            Fields::Named(n) => {
                let mut fieldnames = vec![];
                for field in n.named.iter() {
                    if has_attr(&field.attrs, "skip_py") {
                        continue;
                    }
                    fieldnames.push(field.ident.as_ref().unwrap());
                }
                let kwargs_toks = fields_to_kwargs(&var.fields, true);
                toks.push(quote! {
                    Self::#varname { #(#fieldnames,)* .. } => {
                        use pyo3::types::PyAnyMethods;

                        let libcst = pyo3::types::PyModule::import_bound(py, "libcst")?;
                        let kwargs = #kwargs_toks ;
                        Ok(libcst
                            .getattr(stringify!(#varname))
                            .expect(stringify!(no #varname found in libcst))
                            .call((), Some(&kwargs))?
                            .into())
                    }
                })
            }
            f @ Fields::Unit => {
                return quote_spanned! {
                    f.span() =>
                    compile_error!("Empty enum variants not supported")
                }
                .into()
            }
            Fields::Unnamed(_) => {
                toks.push(quote! {
                    Self::#varname(x, ..) => x.try_into_py(py),
                });
            }
        }
    }
    let ident = &ast.ident;
    let generics = &ast.generics;
    let gen = quote! {
        use pyo3::types::IntoPyDict as _;
        #[automatically_derived]
        impl#generics crate::nodes::traits::py::TryIntoPy<pyo3::PyObject> for #ident #generics {
            fn try_into_py(self, py: pyo3::Python) -> pyo3::PyResult<pyo3::PyObject> {
                match self {
                    #(#toks)*
                }
            }
        }
    };
    gen.into()
}

fn impl_into_py_struct(ast: &DeriveInput, e: &DataStruct) -> TokenStream {
    let kwargs_toks = fields_to_kwargs(&e.fields, false);
    let ident = &ast.ident;
    let generics = &ast.generics;
    let gen = quote! {
        use pyo3::types::IntoPyDict as _;
        #[automatically_derived]
        impl#generics crate::nodes::traits::py::TryIntoPy<pyo3::PyObject> for #ident #generics {
            fn try_into_py(self, py: pyo3::Python) -> pyo3::PyResult<pyo3::PyObject> {
                use pyo3::types::PyAnyMethods;
                let libcst = pyo3::types::PyModule::import_bound(py, "libcst")?;
                let kwargs = #kwargs_toks ;
                Ok(libcst
                    .getattr(stringify!(#ident))
                    .expect(stringify!(no #ident found in libcst))
                    .call((), Some(&kwargs))?
                    .into())
            }
        }
    };
    gen.into()
}

fn fields_to_kwargs(fields: &Fields, is_enum: bool) -> quote::__private::TokenStream {
    let mut empty_kwargs = false;
    let mut py_varnames = vec![];
    let mut rust_varnames = vec![];
    let mut optional_py_varnames = vec![];
    let mut optional_rust_varnames = vec![];
    match &fields {
        Fields::Named(FieldsNamed { named, .. }) => {
            for field in named.iter() {
                if has_attr(&field.attrs, "skip_py") {
                    continue;
                }
                if let Some(ident) = field.ident.as_ref() {
                    let include = if let Visibility::Public(_) = field.vis {
                        true
                    } else {
                        is_enum
                    };
                    if include {
                        let pyname = format_ident!("{}", ident);
                        let rustname = if is_enum {
                            ident.to_token_stream()
                        } else {
                            quote! { self.#ident }
                        };
                        if !has_attr(&field.attrs, "no_py_default") {
                            if let Type::Path(TypePath { path, .. }) = &field.ty {
                                if let Some(first) = path.segments.first() {
                                    if first.ident == "Option" {
                                        optional_py_varnames.push(pyname);
                                        optional_rust_varnames.push(rustname);
                                        continue;
                                    }
                                }
                            }
                        }
                        py_varnames.push(pyname);
                        rust_varnames.push(rustname);
                    }
                }
            }
            empty_kwargs = py_varnames.is_empty() && optional_py_varnames.is_empty()
        }
        Fields::Unnamed(FieldsUnnamed { unnamed, .. }) => {
            if unnamed.first().is_some() {
                py_varnames.push(format_ident!("value"));
                rust_varnames.push(quote! { self.0 });
            } else {
                empty_kwargs = true;
            }
        }
        Fields::Unit => {
            empty_kwargs = true;
        }
    };
    let kwargs_pairs = quote! {
        #(Some((stringify!(#py_varnames), #rust_varnames.try_into_py(py)?)),)*
    };
    let optional_pairs = quote! {
        #(#optional_rust_varnames.map(|x| x.try_into_py(py)).transpose()?.map(|x| (stringify!(#optional_py_varnames), x)),)*
    };
    if empty_kwargs {
        quote! { pyo3::types::PyDict::new_bound(py) }
    } else {
        quote! {
            [ #kwargs_pairs #optional_pairs ]
                .iter()
                .filter(|x| x.is_some())
                .map(|x| x.as_ref().unwrap())
                .collect::<Vec<_>>()
                .into_py_dict_bound(py)
        }
    }
}

fn has_attr(attrs: &[Attribute], name: &'static str) -> bool {
    attrs.iter().any(|attr| attr.path().is_ident(name))
}
