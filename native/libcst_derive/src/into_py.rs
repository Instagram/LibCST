use proc_macro::TokenStream;
use quote::{format_ident, quote, quote_spanned};
use syn::{
    spanned::Spanned, Data, DataEnum, DataStruct, DeriveInput, Fields, FieldsNamed, FieldsUnnamed,
    Visibility,
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
                    if field.attrs.iter().any(|attr| attr.path.is_ident("skip_py")) {
                        continue;
                    }
                    fieldnames.push(field.ident.as_ref().unwrap());
                }
                let kwargs_toks = fields_to_kwargs(&var.fields);
                toks.push(quote! {
                    Self::#varname { #(#fieldnames,)* .. } => {
                        let libcst = pyo3::types::PyModule::import(py, "libcst").expect("libcst couldn't be imported");
                        let kwargs = #kwargs_toks ;
                        libcst
                            .getattr(stringify!(#varname))
                            .expect(stringify!(no #varname found in libcst))
                            .call((), Some(kwargs))
                            .expect("conversion failed")
                            .into()
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
                    Self::#varname(x, ..) => x.into_py(py),
                });
            }
        }
    }
    let ident = &ast.ident;
    let generics = &ast.generics;
    let gen = quote! {
        use pyo3::types::IntoPyDict as _;
        #[automatically_derived]
        impl#generics pyo3::conversion::IntoPy<pyo3::PyObject> for #ident #generics {
            fn into_py(self, py: pyo3::Python) -> pyo3::PyObject {
                match self {
                    #(#toks)*
                }
            }
        }
    };
    gen.into()
}

fn impl_into_py_struct(ast: &DeriveInput, e: &DataStruct) -> TokenStream {
    let kwargs_toks = fields_to_kwargs(&e.fields);
    let ident = &ast.ident;
    let generics = &ast.generics;
    let gen = quote! {
        use pyo3::types::IntoPyDict as _;
        #[automatically_derived]
        impl#generics pyo3::conversion::IntoPy<pyo3::PyObject> for #ident #generics {
            fn into_py(self, py: pyo3::Python) -> pyo3::PyObject {
                let libcst = pyo3::types::PyModule::import(py, "libcst").expect("libcst couldn't be imported");
                let kwargs = #kwargs_toks ;
                libcst
                    .getattr(stringify!(#ident))
                    .expect(stringify!(no #ident found in libcst))
                    .call((), Some(kwargs))
                    .expect("conversion failed")
                    .into()
            }
        }
    };
    gen.into()
}

fn fields_to_kwargs(fields: &Fields) -> quote::__private::TokenStream {
    let mut empty_kwargs = false;
    let kwargs_pairs = match &fields {
        Fields::Named(FieldsNamed { named, .. }) => {
            let mut py_varnames = vec![];
            let mut rust_varnames = vec![];
            for field in named.iter() {
                if field.attrs.iter().any(|attr| attr.path.is_ident("skip_py")) {
                    continue;
                }
                if let Some(ident) = field.ident.as_ref() {
                    if let Visibility::Public(_) = field.vis {
                        py_varnames.push(format_ident!("{}", ident));
                        rust_varnames.push(ident);
                    }
                }
            }
            empty_kwargs = py_varnames.is_empty();
            quote! {
                #((stringify!(#py_varnames), self.#rust_varnames.into_py(py)),)*
            }
        }
        Fields::Unnamed(FieldsUnnamed { unnamed, .. }) => {
            if unnamed.first().is_some() {
                quote! {
                    ("value", self.0.into_py(py)),
                }
            } else {
                empty_kwargs = true;
                quote! {}
            }
        }
        Fields::Unit => quote! {},
    };
    if empty_kwargs {
        quote! { pyo3::types::PyDict::new(py) }
    } else {
        quote! { [ #kwargs_pairs ].into_py_dict(py) }
    }
}
