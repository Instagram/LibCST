// Copyright (c) Meta Platforms, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree

use proc_macro::{Span, TokenStream};
use quote::{format_ident, quote, quote_spanned, ToTokens};
use syn::{
    self,
    punctuated::{Pair, Punctuated},
    spanned::Spanned,
    token::Comma,
    AngleBracketedGenericArguments, Attribute, Data, DataEnum, DataStruct, DeriveInput, Field,
    Fields, FieldsNamed, FieldsUnnamed, GenericArgument, GenericParam, Generics, Ident, Lifetime,
    LifetimeDef, ParenthesizedGenericArguments, Path, PathArguments, PathSegment, Token, Type,
    TypePath, Visibility,
};

pub(crate) fn impl_cst_node(ast: DeriveInput) -> TokenStream {
    match ast.data {
        Data::Enum(e) => impl_enum(ast.attrs, ast.vis, ast.ident, ast.generics, e),
        Data::Struct(s) => impl_struct(ast.attrs, ast.vis, ast.ident, ast.generics, s),
        Data::Union(u) => quote_spanned! {
            u.union_token.span() =>
            compile_error!("Union type is not supported")
        }
        .into(),
    }
}

// enum Foo<'a> {
//     Variant(Variant<'a>),
// }
// =>
// enum Foo<'a> {
//     Variant(Variant<'a>),
// }
// enum DeflatedFoo<'r, 'a> {
//     Variant(DeflatedVariant<'r, 'a>),
// }

fn impl_enum(
    attrs: Vec<Attribute>,
    vis: Visibility,
    ident: Ident,
    generics: Generics,
    mut e: DataEnum,
) -> TokenStream {
    let deflated_ident = format_ident!("Deflated{}", &ident);
    let mut deflated_generics = generics.clone();
    let mut added_lifetime = false;
    let mut deflated_variant_tokens = vec![];

    for var in e.variants.iter_mut() {
        let (inflated_fields, deflated_fields, extra_lifetime) = impl_fields(var.fields.clone());
        added_lifetime |= extra_lifetime;
        var.fields = deflated_fields;
        deflated_variant_tokens.push(var.to_token_stream());
        var.fields = inflated_fields;
    }
    if added_lifetime {
        deflated_generics.params.insert(
            0,
            GenericParam::Lifetime(LifetimeDef::new(Lifetime::new(
                "'r",
                Span::call_site().into(),
            ))),
        );
    }
    let inflated = DeriveInput {
        attrs,
        vis,
        ident,
        generics,
        data: Data::Enum(e),
    };

    let gen = quote! {
        #inflated
        enum #deflated_ident#deflated_generics {
            #(#deflated_variant_tokens)*
        }
    };
    gen.into()
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
    attrs: Vec<Attribute>,
    vis: Visibility,
    ident: Ident,
    generics: Generics,
    mut s: DataStruct,
) -> TokenStream {
    let deflated_ident = format_ident!("Deflated{}", &ident);
    let mut deflated_generics = generics.clone();

    let (inflated_fields, deflated_fields, added_lifetime) = impl_fields(s.fields);
    s.fields = inflated_fields;
    if added_lifetime {
        deflated_generics.params.insert(
            0,
            GenericParam::Lifetime(LifetimeDef::new(Lifetime::new(
                "'r",
                Span::call_site().into(),
            ))),
        );
    }
    let inflated = DeriveInput {
        attrs,
        vis,
        ident,
        generics,
        data: Data::Struct(s),
    };

    let gen = quote! {
        #inflated

        struct #deflated_ident#deflated_generics
        #deflated_fields

    };
    gen.into()
}

fn impl_fields(fields: Fields) -> (Fields, Fields, bool) {
    match &fields {
        Fields::Unnamed(fs) => {
            let (deflated_fields, added_lifetime) = impl_unnamed_fields(fs.clone());
            (fields, Fields::Unnamed(deflated_fields), added_lifetime)
        }
        Fields::Named(fs) => impl_named_fields(fs.clone()),
        Fields::Unit => (Fields::Unit, Fields::Unit, false),
    }
}

fn impl_unnamed_fields(mut deflated_fields: FieldsUnnamed) -> (FieldsUnnamed, bool) {
    let mut added_lifetime = false;
    for f in deflated_fields.unnamed.iter_mut() {
        if let Type::Path(TypePath { path, .. }) = &mut f.ty {
            if let Some(seg) = path.segments.last_mut() {
                seg.ident = format_ident!("Deflated{}", seg.ident);
                if let PathArguments::AngleBracketed(AngleBracketedGenericArguments {
                    args, ..
                }) = &mut seg.arguments
                {
                    added_lifetime = true;
                    args.insert(
                        0,
                        GenericArgument::Lifetime(Lifetime::new("'r", Span::call_site().into())),
                    );
                }
            }
        }
    }
    (deflated_fields, added_lifetime)
}

fn impl_named_fields(mut fields: FieldsNamed) -> (Fields, Fields, bool) {
    let mut deflated_fields = fields.clone();
    let mut added_lifetime = false;
    let span: Span = Span::call_site();
    // Drop whitespace fields from deflated fields
    // And add lifetimes to tokenref fields
    deflated_fields.named = deflated_fields
        .named
        .into_pairs()
        .filter(|pair| {
            let id = pair.value().ident.as_ref().unwrap();
            !format!("{}", id).starts_with("whitespace_")
        })
        .map(|pair| {
            added_lifetime = true;
            add_lifetimes(pair, span)
        })
        .collect();

    // Drop tokenref fields from inflated fields
    fields.named = fields
        .named
        .into_pairs()
        .filter(|pair| !is_token_ref(pair.value()))
        .collect();
    (
        Fields::Named(fields),
        Fields::Named(deflated_fields),
        added_lifetime,
    )
}

fn is_token_ref(field: &Field) -> bool {
    if let Type::Path(path) = &field.ty {
        if let Some(id) = path.path.segments.last().map(|seg| &seg.ident) {
            return format!("{}", id) == "TokenRef";
        }
    }
    false
}

// foo::bar -> foo::bar<'r, 'a>
fn add_lifetimes(mut pair: Pair<Field, Comma>, span: Span) -> Pair<Field, Comma> {
    if let Some(seg) = rightmost_path_segment_mut(&mut pair.value_mut().ty) {
        let lifetime_argument = GenericArgument::Lifetime(Lifetime::new("'r", span.into()));
        match seg.arguments {
            PathArguments::None => {
                let mut generic_args = Punctuated::<_, _>::new();
                generic_args.push(lifetime_argument);
                seg.arguments = PathArguments::AngleBracketed(AngleBracketedGenericArguments {
                    colon2_token: None,
                    lt_token: Token![<]([span.into()]),
                    gt_token: Token![>]([span.into()]),
                    args: generic_args,
                })
            }
            PathArguments::AngleBracketed(AngleBracketedGenericArguments {
                ref mut args, ..
            }) => {
                args.insert(0, lifetime_argument);
            }
            _ => todo!(),
        }
    }
    pair
}

// fn rightmost_path_segment(ty: &Type) -> Option<&PathSegment> {
//     if let Type::Path(TypePath { path, .. }) = ty {
//         if let Some(seg) = path.segments.last() {
//             if let PathArguments::AngleBracketed(AngleBracketedGenericArguments { args, .. }) =
//                 &seg.arguments
//             {
//                 if let Some(GenericArgument::Type(t)) = args.last() {
//                     return rightmost_path_segment(t);
//                 }
//             }
//             return Some(seg);
//         }
//     }
//     None
// }

type Link = Option<Box<Node>>;

struct Node {
    next: Link,
}

struct Recursive {
    root: Link,
}

// fn back(node: &mut Node) -> &mut Link {
//     let mut anchor = &mut Some(Box::new(node));
//     loop {
//         match { anchor } {
//             &mut Some(ref mut node) => anchor = &mut node.next,
//             other => return other,
//         }
//     }
// }

fn back(start: &mut Link) -> &mut Link {
    let mut anchor = start;

    loop {
        match { anchor } {
            &mut Some(ref mut node) => anchor = &mut node.next,
            other => return other,
        }
    }
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

// fn rightmost_path_segment_mut(mut ty: &mut Type) -> Option<&mut PathSegment> {
//     loop {
//         match { &mut *ty } {
//             Type::Path(TypePath { path, .. }) => {
//                 let last_seg = path.segments.last_mut().unwrap();
//                 match { &mut *last_seg }.arguments {
//                     PathArguments::AngleBracketed(AngleBracketedGenericArguments {
//                         mut args,
//                         ..
//                     }) => match args.last_mut().unwrap() {
//                         GenericArgument::Type(t) => ty = t,
//                         _ => {}
//                     },
//                     _ => return None,
//                 }
//             }
//             _ => return None,
//         }
//     }
//     //     let tmp = ret;
//     //     if let Some(ref mut seg) = *tmp {
//     //         if let PathArguments::AngleBracketed(AngleBracketedGenericArguments { args, .. }) =
//     //             &mut seg.arguments
//     //         {
//     //             if let Some(GenericArgument::Type(t)) = args.last_mut() {
//     //                 ty_ = Some(t);
//     //                 continue;
//     //             }
//     //         }
//     //     }
//     //     ty_ = None;
//     // }
//     // return ret;

//     // if let Type::Path(TypePath { path, .. }) = ty {
//     //     if let Some(seg) = { path.segments.last_mut() } {
//     //         if let PathArguments::AngleBracketed(AngleBracketedGenericArguments { args, .. }) =
//     //             &mut { seg }.arguments
//     //         {
//     //             if let Some(GenericArgument::Type(t)) = args.last_mut() {
//     //                 return rightmost_path_segment_mut(t);
//     //             }
//     //         }
//     //         return Some(seg);
//     //     }
//     // }
//     // None
// }
