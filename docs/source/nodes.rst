Nodes
=====

CSTNode and it's subclasses cover Python's full grammar in a
whitespace-sensitive fashion, forming LibCST's concrete syntax tree.

Many of these nodes are designed to `behave similarly to Python's abstract
syntax tree <https://greentreesnakes.readthedocs.io/en/latest/nodes.html>`_.

CSTNode
-------

.. autoclass:: libcst.CSTNode

Module
------

.. autoclass:: libcst.Module

Expressions
-----------

.. autoclass:: libcst.Annotation
.. autoclass:: libcst.Arg
.. autoclass:: libcst.Asynchronous
.. autoclass:: libcst.Attribute
.. autoclass:: libcst.Await
.. autoclass:: libcst.BaseAtom
.. autoclass:: libcst.BaseComp
.. autoclass:: libcst.BaseDict
.. autoclass:: libcst.BaseDictElement
.. autoclass:: libcst.BaseElement
.. autoclass:: libcst.BaseExpression
.. autoclass:: libcst.BaseFormattedStringContent
.. autoclass:: libcst.BaseList
.. autoclass:: libcst.BaseNumber
.. autoclass:: libcst.BaseSet
.. autoclass:: libcst.BaseString
.. autoclass:: libcst.BinaryOperation
.. autoclass:: libcst.BooleanOperation
.. autoclass:: libcst.Call
.. autoclass:: libcst.Comparison
.. autoclass:: libcst.ComparisonTarget
.. autoclass:: libcst.CompFor
.. autoclass:: libcst.CompIf
.. autoclass:: libcst.ConcatenatedString
.. autoclass:: libcst.Dict
.. .. autoclass:: libcst.DictComp
.. autoclass:: libcst.DictElement
.. autoclass:: libcst.Element
.. autoclass:: libcst.Ellipses
.. autoclass:: libcst.ExtSlice
.. autoclass:: libcst.Float
.. autoclass:: libcst.FormattedString
.. autoclass:: libcst.FormattedStringExpression
.. autoclass:: libcst.FormattedStringText
.. autoclass:: libcst.From
.. autoclass:: libcst.GeneratorExp
.. autoclass:: libcst.IfExp
.. autoclass:: libcst.Imaginary
.. autoclass:: libcst.Index
.. autoclass:: libcst.Integer
.. autoclass:: libcst.Lambda
.. autoclass:: libcst.LeftCurlyBrace
.. autoclass:: libcst.LeftParen
.. autoclass:: libcst.LeftSquareBracket
.. autoclass:: libcst.List
.. autoclass:: libcst.ListComp
.. autoclass:: libcst.Name
.. autoclass:: libcst.Param
.. autoclass:: libcst.Parameters
.. autoclass:: libcst.ParamStar
.. autoclass:: libcst.RightCurlyBrace
.. autoclass:: libcst.RightParen
.. autoclass:: libcst.RightSquareBracket
.. autoclass:: libcst.Set
.. autoclass:: libcst.SetComp
.. autoclass:: libcst.SimpleString
.. autoclass:: libcst.Slice
.. autoclass:: libcst.StarredDictElement
.. autoclass:: libcst.StarredElement
.. autoclass:: libcst.Subscript
.. autoclass:: libcst.Tuple
.. autoclass:: libcst.UnaryOperation
.. autoclass:: libcst.Yield

Statements
----------

.. autoclass:: libcst.AnnAssign
.. autoclass:: libcst.AsName
.. autoclass:: libcst.Assert
.. autoclass:: libcst.Assign
.. autoclass:: libcst.AssignTarget
.. autoclass:: libcst.AugAssign
.. autoclass:: libcst.BaseCompoundStatement
.. autoclass:: libcst.BaseSmallStatement
.. autoclass:: libcst.BaseSuite
.. autoclass:: libcst.Break
.. autoclass:: libcst.ClassDef
.. autoclass:: libcst.Continue
.. autoclass:: libcst.Decorator
.. autoclass:: libcst.Del
.. autoclass:: libcst.Else
.. autoclass:: libcst.ExceptHandler
.. autoclass:: libcst.Expr
.. autoclass:: libcst.Finally
.. autoclass:: libcst.For
.. autoclass:: libcst.FunctionDef
.. autoclass:: libcst.Global
.. autoclass:: libcst.If
.. autoclass:: libcst.Import
.. autoclass:: libcst.ImportAlias
.. autoclass:: libcst.ImportFrom
.. autoclass:: libcst.IndentedBlock
.. autoclass:: libcst.NameItem
.. autoclass:: libcst.Nonlocal
.. autoclass:: libcst.Pass
.. autoclass:: libcst.Raise
.. autoclass:: libcst.Return
.. autoclass:: libcst.SimpleStatementLine
.. autoclass:: libcst.SimpleStatementSuite
.. autoclass:: libcst.Try
.. autoclass:: libcst.While
.. autoclass:: libcst.With
.. autoclass:: libcst.WithItem

Operators
---------

.. autoclass:: libcst.Add
.. autoclass:: libcst.AddAssign
.. autoclass:: libcst.And
.. autoclass:: libcst.AssignEqual
.. autoclass:: libcst.BaseAugOp
.. autoclass:: libcst.BaseBinaryOp
.. autoclass:: libcst.BaseBooleanOp
.. autoclass:: libcst.BaseCompOp
.. autoclass:: libcst.BaseUnaryOp
.. autoclass:: libcst.BitAnd
.. autoclass:: libcst.BitAndAssign
.. autoclass:: libcst.BitInvert
.. autoclass:: libcst.BitOr
.. autoclass:: libcst.BitOrAssign
.. autoclass:: libcst.BitXor
.. autoclass:: libcst.BitXorAssign
.. autoclass:: libcst.Colon
.. autoclass:: libcst.Comma
.. autoclass:: libcst.Divide
.. autoclass:: libcst.DivideAssign
.. autoclass:: libcst.Dot
.. autoclass:: libcst.Equal
.. autoclass:: libcst.FloorDivide
.. autoclass:: libcst.FloorDivideAssign
.. autoclass:: libcst.GreaterThan
.. autoclass:: libcst.GreaterThanEqual
.. autoclass:: libcst.ImportStar
.. autoclass:: libcst.In
.. autoclass:: libcst.Is
.. autoclass:: libcst.IsNot
.. autoclass:: libcst.LeftShift
.. autoclass:: libcst.LeftShiftAssign
.. autoclass:: libcst.LessThan
.. autoclass:: libcst.LessThanEqual
.. autoclass:: libcst.MatrixMultiply
.. autoclass:: libcst.MatrixMultiplyAssign
.. autoclass:: libcst.Minus
.. autoclass:: libcst.Modulo
.. autoclass:: libcst.ModuloAssign
.. autoclass:: libcst.Multiply
.. autoclass:: libcst.MultiplyAssign
.. autoclass:: libcst.Not
.. autoclass:: libcst.NotEqual
.. autoclass:: libcst.NotIn
.. autoclass:: libcst.Or
.. autoclass:: libcst.Plus
.. autoclass:: libcst.Power
.. autoclass:: libcst.PowerAssign
.. autoclass:: libcst.RightShift
.. autoclass:: libcst.RightShiftAssign
.. autoclass:: libcst.Semicolon
.. autoclass:: libcst.Subtract
.. autoclass:: libcst.SubtractAssign

Whitespace
----------

.. autoclass:: libcst.Comment
.. autoclass:: libcst.EmptyLine
.. autoclass:: libcst.Newline
.. autoclass:: libcst.ParenthesizedWhitespace
.. autoclass:: libcst.SimpleWhitespace
.. autoclass:: libcst.TrailingWhitespace
.. autoclass:: libcst.BaseParenthesizableWhitespace
