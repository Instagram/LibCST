// Copyright (c) Meta Platforms, Inc. and affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree

use proc_macro::TokenStream;
use quote::{format_ident, quote, quote_spanned, ToTokens};
use syn::{
    self,
    parse::{Parse, ParseStream},
    parse_quote,
    punctuated::{Pair, Punctuated},
    spanned::Spanned,
    token::Comma,
    AngleBracketedGenericArguments, Attribute, Data, DataEnum, DataStruct, DeriveInput, Field,
    Fields, FieldsNamed, FieldsUnnamed, GenericArgument, Generics, Ident, Meta, Path,
    PathArguments, PathSegment, Token, Type, TypePath, Visibility,
};

pub(crate) struct CSTNodeParams {
    traits: Punctuated<SupportedTrait, Token![,]>,
}

#[derive(PartialEq, Eq)]
enum SupportedTrait {
    ParenthesizedNode,
    Codegen,
    Inflate,
    NoIntoPy,
    Default,
}

pub(crate) fn impl_cst_node(ast: DeriveInput, args: CSTNodeParams) -> TokenStream {
    match ast.data {
        Data::Enum(e) => impl_enum(args, ast.attrs, ast.vis, ast.ident, ast.generics, e),
        Data::Struct(s) => impl_struct(args, ast.attrs, ast.vis, ast.ident, ast.generics, s),
        Data::Union(u) => quote_spanned! {
            u.union_token.span() =>
            compile_error!("Union type is not supported")
        }
        .into(),
    }
}

impl CSTNodeParams {
    fn has_trait(&self, treyt: &SupportedTrait) -> bool {
        self.traits.iter().any(|x| x == treyt)
    }
}

impl Parse for SupportedTrait {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.peek(Ident) {
            let id: Ident = input.parse()?;
            return match id.to_string().as_str() {
                "ParenthesizedNode" => Ok(Self::ParenthesizedNode),
                "Codegen" => Ok(Self::Codegen),
                "Inflate" => Ok(Self::Inflate),
                "NoIntoPy" => Ok(Self::NoIntoPy),
                "Default" => Ok(Self::Default),
                _ => Err(input.error("Not a supported trait to derive for cst_node")),
            };
        }
        Err(input.error("Pass in trait names to be derived"))
    }
}

impl Parse for CSTNodeParams {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self {
            traits: input.parse_terminated(SupportedTrait::parse, Token![,])?,
        })
    }
}

// enum Foo<'a> {
//     Variant(Box<Variant<'a>>),
// }
// =>
// enum Foo<'a> {
//     Variant(Box<Variant<'a>>),
// }
// enum DeflatedFoo<'r, 'a> {
//     Variant(Box<DeflatedVariant<'r, 'a>>),
// }

fn impl_enum(
    args: CSTNodeParams,
    mut attrs: Vec<Attribute>,
    vis: Visibility,
    ident: Ident,
    generics: Generics,
    mut e: DataEnum,
) -> TokenStream {
    let deflated_vis = vis.clone();
    let deflated_ident = format_ident!("Deflated{}", &ident);
    let deflated_generics: Generics = parse_quote!(<'r, 'a>);
    let mut deflated_variant_tokens = vec![];

    for var in e.variants.iter_mut() {
        let (inflated_fields, deflated_fields) = impl_fields(var.fields.clone());
        var.fields = deflated_fields;
        deflated_variant_tokens.push(var.to_token_stream());
        var.fields = inflated_fields;
    }
    add_inflated_attrs(&args, &mut attrs);
    let inflated = DeriveInput {
        attrs,
        vis,
        ident,
        generics,
        data: Data::Enum(e),
    };

    let deflated_attrs = get_deflated_attrs(&args);

    let gen = quote! {
        #[derive(Debug, PartialEq, Eq, Clone)]
        #inflated

        #[derive(Debug, PartialEq, Eq, Clone)]
        #(#deflated_attrs)*
        #deflated_vis enum #deflated_ident#deflated_generics {
            #(#deflated_variant_tokens,)*
        }
    };
    gen.into()
}

fn get_deflated_attrs(args: &CSTNodeParams) -> Vec<Attribute> {
    let mut deflated_attrs: Vec<Attribute> = vec![];
    if args.has_trait(&SupportedTrait::Inflate) {
        deflated_attrs.push(parse_quote!(#[derive(Inflate)]));
    }
    if args.has_trait(&SupportedTrait::ParenthesizedNode) {
        deflated_attrs.push(parse_quote!(#[derive(ParenthesizedDeflatedNode)]))
    }
    if args.has_trait(&SupportedTrait::Default) {
        deflated_attrs.push(parse_quote!(#[derive(Default)]));
    }
    deflated_attrs
}

fn add_inflated_attrs(args: &CSTNodeParams, attrs: &mut Vec<Attribute>) {
    if args.has_trait(&SupportedTrait::Codegen) {
        attrs.push(parse_quote!(#[derive(Codegen)]));
    }
    if args.has_trait(&SupportedTrait::ParenthesizedNode) {
        attrs.push(parse_quote!(#[derive(ParenthesizedNode)]));
    }
    if args.has_trait(&SupportedTrait::Default) {
        attrs.push(parse_quote!(#[derive(Default)]));
    }
    if !args.has_trait(&SupportedTrait::NoIntoPy) {
        attrs.push(parse_quote!(#[cfg_attr(feature = "py", derive(TryIntoPy))]));
    }
}

// pub struct Foo<'a> {
//     pub bar: Bar<'a>,
//     pub value: &'a str,
//     pub whitespace_after: SimpleWhitespace<'a>,
//     pub(crate) tok: Option<TokenRef>,
// }
// =>
// pub struct Foo<'a> {
//     pub bar: Bar<'a>,
//     pub value: &'a str,
//     pub whitespace_after: SimpleWhitespace<'a>,
// }
// struct DeflatedFoo<'r, 'a> {
//     pub bar: DeflatedBar<'r, 'a>,
//     pub value: &'a str,
//     pub tok: Option<TokenRef<'r, 'a>>
// }

fn impl_struct(
    args: CSTNodeParams,
    mut attrs: Vec<Attribute>,
    vis: Visibility,
    ident: Ident,
    generics: Generics,
    mut s: DataStruct,
) -> TokenStream {
    let deflated_vis = vis.clone();
    let deflated_ident = format_ident!("Deflated{}", &ident);
    let deflated_generics: Generics = parse_quote!(<'r, 'a>);

    let (inflated_fields, deflated_fields) = impl_fields(s.fields);
    s.fields = inflated_fields;

    add_inflated_attrs(&args, &mut attrs);

    let inflated = DeriveInput {
        attrs,
        vis,
        ident,
        generics,
        data: Data::Struct(s),
    };

    let deflated_attrs = get_deflated_attrs(&args);

    let gen = quote! {
        #[derive(Debug, PartialEq, Eq, Clone)]
        #inflated

        #[derive(Debug, PartialEq, Eq, Clone)]
        #(#deflated_attrs)*
        #deflated_vis struct #deflated_ident#deflated_generics
        #deflated_fields

    };
    gen.into()
}

fn impl_fields(fields: Fields) -> (Fields, Fields) {
    match &fields {
        Fields::Unnamed(fs) => {
            let deflated_fields = impl_unnamed_fields(fs.clone());
            (fields, Fields::Unnamed(deflated_fields))
        }
        Fields::Named(fs) => impl_named_fields(fs.clone()),
        Fields::Unit => (Fields::Unit, Fields::Unit),
    }
}

fn impl_unnamed_fields(mut deflated_fields: FieldsUnnamed) -> FieldsUnnamed {
    let mut added_lifetime = false;
    deflated_fields.unnamed = deflated_fields
        .unnamed
        .into_pairs()
        .map(|pair| {
            let (deflated, lifetime) = make_into_deflated(pair);
            added_lifetime |= lifetime;
            deflated
        })
        .collect();

    // Make sure all Deflated* types have 'r 'a lifetime params
    if !added_lifetime {
        deflated_fields.unnamed.push(parse_quote! {
            std::marker::PhantomData<&'r &'a ()>
        });
    }
    deflated_fields
}

fn impl_named_fields(mut fields: FieldsNamed) -> (Fields, Fields) {
    let mut deflated_fields = fields.clone();
    let mut added_lifetime = false;
    // Drop whitespace fields from deflated fields
    // And add lifetimes to tokenref fields
    deflated_fields.named = deflated_fields
        .named
        .into_pairs()
        .filter(|pair| {
            let id = pair.value().ident.as_ref().unwrap().to_string();
            !id.contains("whitespace")
                && id != "footer"
                && id != "header"
                && id != "leading_lines"
                && id != "lines_after_decorators"
        })
        .map(|pair| {
            if is_builtin(pair.value()) {
                pair
            } else {
                let (deflated, lifetime) = make_into_deflated(pair);
                added_lifetime |= lifetime;
                deflated
            }
        })
        .map(|pair| {
            let (mut val, punct) = pair.into_tuple();
            val.attrs = val.attrs.into_iter().filter(is_not_intopy_attr).collect();
            Pair::new(val, punct)
        })
        .collect();

    // Make sure all Deflated* types have 'r 'a lifetime params
    if !added_lifetime {
        deflated_fields.named.push(parse_quote! {
            _phantom: std::marker::PhantomData<&'r &'a ()>
        });
    }

    // Drop tokenref fields from inflated fields
    fields.named = fields
        .named
        .into_pairs()
        .filter(|pair| !is_token_ref(pair.value()))
        .collect();

    (Fields::Named(fields), Fields::Named(deflated_fields))
}

fn is_builtin(field: &Field) -> bool {
    get_pathseg(&field.ty)
        .map(|seg| {
            let segstr = seg.ident.to_string();
            segstr == "str" || segstr == "bool" || segstr == "String"
        })
        .unwrap_or_default()
}

fn is_token_ref(field: &Field) -> bool {
    if let Some(seg) = rightmost_path_segment(&field.ty) {
        return format!("{}", seg.ident) == "TokenRef";
    }
    false
}

// foo::bar -> foo::Deflatedbar<'r, 'a>
fn make_into_deflated(mut pair: Pair<Field, Comma>) -> (Pair<Field, Comma>, bool) {
    let mut added_lifetime = true;
    if let Some(seg) = rightmost_path_segment_mut(&mut pair.value_mut().ty) {
        let seg_name = seg.ident.to_string();
        if seg_name != "TokenRef" {
            seg.ident = format_ident!("Deflated{}", seg_name);
        }
        match seg.arguments {
            PathArguments::None => {
                seg.arguments = PathArguments::AngleBracketed(parse_quote!(<'r, 'a>));
            }
            PathArguments::AngleBracketed(AngleBracketedGenericArguments {
                ref mut args, ..
            }) => {
                args.insert(0, parse_quote!('r));
            }
            _ => todo!(),
        }
    } else {
        added_lifetime = false;
    }
    (pair, added_lifetime)
}

// foo::bar::baz<quux<'a>> -> baz<quux<'a>>
fn get_pathseg(ty: &Type) -> Option<&PathSegment> {
    match ty {
        Type::Path(TypePath { path, .. }) => path.segments.last(),
        _ => None,
    }
}

// foo::bar::baz<quux<'a>> -> quux<'a>
fn rightmost_path_segment(ty: &Type) -> Option<&PathSegment> {
    let mut candidate = get_pathseg(ty);
    loop {
        if let Some(pathseg) = candidate {
            if let PathArguments::AngleBracketed(AngleBracketedGenericArguments { args, .. }) =
                &pathseg.arguments
            {
                if let Some(GenericArgument::Type(t)) = args.last() {
                    candidate = get_pathseg(t);
                    continue;
                }
            }
        }
        break;
    }
    candidate
}

fn get_pathseg_mut(ty: &mut Type) -> Option<&mut PathSegment> {
    match ty {
        Type::Path(TypePath { path, .. }) => path.segments.last_mut(),
        _ => None,
    }
}

fn has_more_mut(candidate: &Option<&mut PathSegment>) -> bool {
    if let Some(PathArguments::AngleBracketed(AngleBracketedGenericArguments {
        ref args, ..
    })) = candidate.as_ref().map(|c| &c.arguments)
    {
        matches!(args.last(), Some(GenericArgument::Type(_)))
    } else {
        false
    }
}

fn rightmost_path_segment_mut(ty: &mut Type) -> Option<&mut PathSegment> {
    let mut candidate = get_pathseg_mut(ty);

    while has_more_mut(&candidate) {
        candidate = match candidate.unwrap().arguments {
            PathArguments::AngleBracketed(AngleBracketedGenericArguments {
                ref mut args, ..
            }) => {
                if let Some(GenericArgument::Type(t)) = args.last_mut() {
                    get_pathseg_mut(t)
                } else {
                    unreachable!();
                }
            }
            _ => unreachable!(),
        };
    }

    candidate
}

fn is_not_intopy_attr(attr: &Attribute) -> bool {
    let path = attr.path();
    // support #[cfg_attr(feature = "py", skip_py)]
    if path.is_ident("cfg_attr") {
        return match attr.parse_args_with(|input: ParseStream| {
            let _: Meta = input.parse()?;
            let _: Token![,] = input.parse()?;
            let nested_path: Path = input.parse()?;
            let _: Option<Token![,]> = input.parse()?;
            Ok(nested_path)
        }) {
            Ok(nested_path) => !is_intopy_attr_path(&nested_path),
            Err(_) => false,
        };
    }
    !is_intopy_attr_path(path)
}

fn is_intopy_attr_path(path: &Path) -> bool {
    path.is_ident("skip_py") || path.is_ident("no_py_default")
}

#[test]
fn trybuild() {
    let t = trybuild::TestCases::new();
    t.pass("tests/pass/*.rs");
}

#[test]
fn test_is_not_intopy_attr() {
    assert!(!is_not_intopy_attr(&parse_quote!(#[skip_py])));
    assert!(!is_not_intopy_attr(&parse_quote!(#[no_py_default])));
    assert!(!is_not_intopy_attr(
        &parse_quote!(#[cfg_attr(foo="bar",skip_py)])
    ));
    assert!(!is_not_intopy_attr(
        &parse_quote!(#[cfg_attr(foo="bar",no_py_default)])
    ));
    assert!(is_not_intopy_attr(&parse_quote!(#[skippy])));
    assert!(is_not_intopy_attr(
        &parse_quote!(#[cfg_attr(foo="bar",skippy)])
    ));
}
