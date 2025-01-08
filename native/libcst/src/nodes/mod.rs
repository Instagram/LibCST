// Copyright (c) Meta Platforms, Inc. and affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree

pub(crate) mod whitespace;
pub use whitespace::{
    Comment, EmptyLine, Fakeness, Newline, ParenthesizableWhitespace, ParenthesizedWhitespace,
    SimpleWhitespace, TrailingWhitespace,
};
pub(crate) mod statement;
pub use statement::{
    AnnAssign, Annotation, AsName, Assert, Assign, AssignTarget, AssignTargetExpression, AugAssign,
    Break, ClassDef, CompoundStatement, Continue, Decorator, Del, DelTargetExpression, Else,
    ExceptHandler, ExceptStarHandler, Expr, Finally, For, FunctionDef, Global, If, Import,
    ImportAlias, ImportFrom, ImportNames, IndentedBlock, Match, MatchAs, MatchCase, MatchClass,
    MatchKeywordElement, MatchList, MatchMapping, MatchMappingElement, MatchOr, MatchOrElement,
    MatchPattern, MatchSequence, MatchSequenceElement, MatchSingleton, MatchStar, MatchTuple,
    MatchValue, NameItem, Nonlocal, OrElse, Pass, Raise, Return, SimpleStatementLine,
    SimpleStatementSuite, SmallStatement, StarrableMatchSequenceElement, Statement, Suite, Try,
    TryStar, TypeAlias, TypeParam, TypeParameters, TypeVar, TypeVarLike, TypeVarTuple, While, With,
    WithItem,
};

pub(crate) mod expression;
pub use expression::{
    Arg, Asynchronous, Attribute, Await, BaseSlice, BinaryOperation, BooleanOperation, Call,
    CompFor, CompIf, Comparison, ComparisonTarget, ConcatenatedString, Dict, DictComp, DictElement,
    Element, Ellipsis, Expression, Float, FormattedString, FormattedStringContent,
    FormattedStringExpression, FormattedStringText, From, GeneratorExp, IfExp, Imaginary, Index,
    Integer, Lambda, LeftCurlyBrace, LeftParen, LeftSquareBracket, List, ListComp, Name,
    NameOrAttribute, NamedExpr, Param, ParamSlash, ParamStar, Parameters, RightCurlyBrace,
    RightParen, RightSquareBracket, Set, SetComp, SimpleString, Slice, StarArg, StarredDictElement,
    StarredElement, String, Subscript, SubscriptElement, Tuple, UnaryOperation, Yield, YieldValue,
};

pub(crate) mod op;
pub use op::{
    AssignEqual, AugOp, BinaryOp, BitOr, BooleanOp, Colon, Comma, CompOp, Dot, ImportStar,
    Semicolon, UnaryOp,
};

pub(crate) mod module;
pub use module::Module;

mod codegen;
pub use codegen::{Codegen, CodegenState};

pub(crate) mod traits;
pub use traits::{Inflate, ParenthesizedNode, WithComma, WithLeadingLines};

pub(crate) mod inflate_helpers;

pub(crate) mod deflated {
    pub use super::expression::{
        DeflatedArg as Arg, DeflatedAsynchronous as Asynchronous, DeflatedAttribute as Attribute,
        DeflatedAwait as Await, DeflatedBaseSlice as BaseSlice,
        DeflatedBinaryOperation as BinaryOperation, DeflatedBooleanOperation as BooleanOperation,
        DeflatedCall as Call, DeflatedCompFor as CompFor, DeflatedCompIf as CompIf,
        DeflatedComparison as Comparison, DeflatedComparisonTarget as ComparisonTarget,
        DeflatedConcatenatedString as ConcatenatedString, DeflatedDict as Dict,
        DeflatedDictComp as DictComp, DeflatedDictElement as DictElement,
        DeflatedElement as Element, DeflatedEllipsis as Ellipsis, DeflatedExpression as Expression,
        DeflatedFloat as Float, DeflatedFormattedString as FormattedString,
        DeflatedFormattedStringContent as FormattedStringContent,
        DeflatedFormattedStringExpression as FormattedStringExpression,
        DeflatedFormattedStringText as FormattedStringText, DeflatedFrom as From,
        DeflatedGeneratorExp as GeneratorExp, DeflatedIfExp as IfExp,
        DeflatedImaginary as Imaginary, DeflatedIndex as Index, DeflatedInteger as Integer,
        DeflatedLambda as Lambda, DeflatedLeftCurlyBrace as LeftCurlyBrace,
        DeflatedLeftParen as LeftParen, DeflatedLeftSquareBracket as LeftSquareBracket,
        DeflatedList as List, DeflatedListComp as ListComp, DeflatedName as Name,
        DeflatedNameOrAttribute as NameOrAttribute, DeflatedNamedExpr as NamedExpr,
        DeflatedParam as Param, DeflatedParamSlash as ParamSlash, DeflatedParamStar as ParamStar,
        DeflatedParameters as Parameters, DeflatedRightCurlyBrace as RightCurlyBrace,
        DeflatedRightParen as RightParen, DeflatedRightSquareBracket as RightSquareBracket,
        DeflatedSet as Set, DeflatedSetComp as SetComp, DeflatedSimpleString as SimpleString,
        DeflatedSlice as Slice, DeflatedStarArg as StarArg,
        DeflatedStarredDictElement as StarredDictElement, DeflatedStarredElement as StarredElement,
        DeflatedString as String, DeflatedSubscript as Subscript,
        DeflatedSubscriptElement as SubscriptElement, DeflatedTuple as Tuple,
        DeflatedUnaryOperation as UnaryOperation, DeflatedYield as Yield,
        DeflatedYieldValue as YieldValue,
    };
    pub use super::module::DeflatedModule as Module;
    pub use super::op::{
        DeflatedAssignEqual as AssignEqual, DeflatedAugOp as AugOp, DeflatedBinaryOp as BinaryOp,
        DeflatedBitOr as BitOr, DeflatedBooleanOp as BooleanOp, DeflatedColon as Colon,
        DeflatedComma as Comma, DeflatedCompOp as CompOp, DeflatedDot as Dot,
        DeflatedImportStar as ImportStar, DeflatedSemicolon as Semicolon,
        DeflatedUnaryOp as UnaryOp,
    };
    pub use super::statement::{
        DeflatedAnnAssign as AnnAssign, DeflatedAnnotation as Annotation, DeflatedAsName as AsName,
        DeflatedAssert as Assert, DeflatedAssign as Assign, DeflatedAssignTarget as AssignTarget,
        DeflatedAssignTargetExpression as AssignTargetExpression, DeflatedAugAssign as AugAssign,
        DeflatedBreak as Break, DeflatedClassDef as ClassDef,
        DeflatedCompoundStatement as CompoundStatement, DeflatedContinue as Continue,
        DeflatedDecorator as Decorator, DeflatedDel as Del,
        DeflatedDelTargetExpression as DelTargetExpression, DeflatedElse as Else,
        DeflatedExceptHandler as ExceptHandler, DeflatedExceptStarHandler as ExceptStarHandler,
        DeflatedExpr as Expr, DeflatedFinally as Finally, DeflatedFor as For,
        DeflatedFunctionDef as FunctionDef, DeflatedGlobal as Global, DeflatedIf as If,
        DeflatedImport as Import, DeflatedImportAlias as ImportAlias,
        DeflatedImportFrom as ImportFrom, DeflatedImportNames as ImportNames,
        DeflatedIndentedBlock as IndentedBlock, DeflatedMatch as Match, DeflatedMatchAs as MatchAs,
        DeflatedMatchCase as MatchCase, DeflatedMatchClass as MatchClass,
        DeflatedMatchKeywordElement as MatchKeywordElement, DeflatedMatchList as MatchList,
        DeflatedMatchMapping as MatchMapping, DeflatedMatchMappingElement as MatchMappingElement,
        DeflatedMatchOr as MatchOr, DeflatedMatchOrElement as MatchOrElement,
        DeflatedMatchPattern as MatchPattern, DeflatedMatchSequence as MatchSequence,
        DeflatedMatchSequenceElement as MatchSequenceElement,
        DeflatedMatchSingleton as MatchSingleton, DeflatedMatchStar as MatchStar,
        DeflatedMatchTuple as MatchTuple, DeflatedMatchValue as MatchValue,
        DeflatedNameItem as NameItem, DeflatedNonlocal as Nonlocal, DeflatedOrElse as OrElse,
        DeflatedParamSpec as ParamSpec, DeflatedPass as Pass, DeflatedRaise as Raise,
        DeflatedReturn as Return, DeflatedSimpleStatementLine as SimpleStatementLine,
        DeflatedSimpleStatementSuite as SimpleStatementSuite,
        DeflatedSmallStatement as SmallStatement,
        DeflatedStarrableMatchSequenceElement as StarrableMatchSequenceElement,
        DeflatedStatement as Statement, DeflatedSuite as Suite, DeflatedTry as Try,
        DeflatedTryStar as TryStar, DeflatedTypeAlias as TypeAlias, DeflatedTypeParam as TypeParam,
        DeflatedTypeParameters as TypeParameters, DeflatedTypeVar as TypeVar,
        DeflatedTypeVarLike as TypeVarLike, DeflatedTypeVarTuple as TypeVarTuple,
        DeflatedWhile as While, DeflatedWith as With, DeflatedWithItem as WithItem,
    };
}
