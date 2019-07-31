Nodes
=====

CSTNode and it's subclasses cover Python's full grammar in a
whitespace-sensitive fashion, forming LibCST's concrete syntax tree.

Many of these nodes are designed to `behave similarly to Python's abstract
syntax tree <https://greentreesnakes.readthedocs.io/en/latest/nodes.html>`_.

CSTNode
-------

The base node type which all other nodes derrive from.

.. autoclass:: libcst.CSTNode

Module
------

A node that represents an entire python module.

.. autoclass:: libcst.Module

Expressions
-----------

Nodes that represent various expressions.

.. autoclass:: libcst.Annotation
.. autoclass:: libcst.Arg
.. autoclass:: libcst.Asynchronous
.. autoclass:: libcst.Attribute
.. autoclass:: libcst.Await
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
.. autoclass:: libcst.DictComp
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

Nodes that represent top-level statements and groups of statements.

Simple Statements
^^^^^^^^^^^^^^^^^

Statements which at most have expressions as child attributes.

.. autoclass:: libcst.BaseSmallStatement
.. autoclass:: libcst.AnnAssign
.. autoclass:: libcst.Assert
.. autoclass:: libcst.Assign
.. autoclass:: libcst.AugAssign
.. autoclass:: libcst.Break
.. autoclass:: libcst.Continue
.. autoclass:: libcst.Del
.. autoclass:: libcst.Expr
.. autoclass:: libcst.Global
.. autoclass:: libcst.Import
.. autoclass:: libcst.ImportFrom
.. autoclass:: libcst.Nonlocal
.. autoclass:: libcst.Pass
.. autoclass:: libcst.Raise
.. autoclass:: libcst.Return


Compound Statements
^^^^^^^^^^^^^^^^^^^

Statements that have one or more statement blocks as a child attribute.

.. autoclass:: libcst.BaseCompoundStatement
.. autoclass:: libcst.ClassDef
.. autoclass:: libcst.For
.. autoclass:: libcst.FunctionDef
.. autoclass:: libcst.If
.. autoclass:: libcst.Try
.. autoclass:: libcst.While
.. autoclass:: libcst.With

Helper Nodes
^^^^^^^^^^^^

Nodes that are used by various statements to represent some syntax, but
are not statements on their own and cannot be used outside of the statements
they blong with.

.. autoclass:: libcst.AsName
.. autoclass:: libcst.AssignTarget
.. autoclass:: libcst.Decorator
.. autoclass:: libcst.Else
.. autoclass:: libcst.ExceptHandler
.. autoclass:: libcst.Finally
.. autoclass:: libcst.ImportAlias
.. autoclass:: libcst.NameItem
.. autoclass:: libcst.WithItem

Statement Blocks
^^^^^^^^^^^^^^^^

Nodes that represent some group of statements.

.. autoclass:: libcst.BaseSuite
.. autoclass:: libcst.SimpleStatementLine
.. autoclass:: libcst.SimpleStatementSuite
.. autoclass:: libcst.IndentedBlock

Operators
---------

Nodes that are used to signify an operation to be performed on a variable
or value.

Unary Operators
^^^^^^^^^^^^^^^

Nodes that are used with :class:`~libcst.UnaryOperation` to perform some unary
operation.

.. class:: libcst.BitInvert
.. class:: libcst.Minus
.. class:: libcst.Not
.. autoclass:: libcst.Plus

In addition, :class:`~libcst.BaseUnaryOp` is defined purely for typing and isinstance
checks.

.. class:: libcst.BaseUnaryOp

Boolean Operators
^^^^^^^^^^^^^^^^^

Nodes that are used with :class:`~libcst.BooleanOperation` to perform some boolean
operation.

.. class:: libcst.And
.. autoclass:: libcst.Or

In addition, :class:`~libcst.BaseBooleanOp` is defined purely for typing and isinstance
checks.

.. class:: libcst.BaseBooleanOp

Binary Operators
^^^^^^^^^^^^^^^^

Nodes that are used with :class:`~libcst.BinaryOperation` to perform some binary
operation.

.. class:: libcst.Add
.. class:: libcst.BitAnd
.. class:: libcst.BitOr
.. class:: libcst.BitXor
.. class:: libcst.Divide
.. class:: libcst.FloorDivide
.. class:: libcst.LeftShift
.. class:: libcst.MatrixMultiply
.. class:: libcst.Modulo
.. class:: libcst.Multiply
.. class:: libcst.Power
.. class:: libcst.RightShift
.. autoclass:: libcst.Subtract

In addition, :class:`~libcst.BaseBinaryOp` is defined purely for typing and isinstance
checks.

.. class:: libcst.BaseBinaryOp

Comparison Operators
^^^^^^^^^^^^^^^^^^^^

Nodes that are used with :class:`~libcst.Comparison` to perform some comparison
operation.

.. class:: libcst.Equal
.. class:: libcst.GreaterThan
.. class:: libcst.GreaterThanEqual
.. class:: libcst.In
.. class:: libcst.Is
.. class:: libcst.LessThan
.. autoclass:: libcst.LessThanEqual

.. autoclass:: libcst.NotEqual

.. class:: libcst.IsNot
.. autoclass:: libcst.NotIn

In addition, :class:`~libcst.BaseCompOp` is defined purely for typing and isinstance
checks.

.. class:: libcst.BaseCompOp

Augmented Assignment Operators
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Nodes that are used with :class:`~libcst.AugAssign` to perform some agumented
assignment.

.. class:: libcst.AddAssign
.. class:: libcst.BitAndAssign
.. class:: libcst.BitOrAssign
.. class:: libcst.BitXorAssign
.. class:: libcst.DivideAssign
.. class:: libcst.FloorDivideAssign
.. class:: libcst.LeftShiftAssign
.. class:: libcst.MatrixMultiplyAssign
.. class:: libcst.ModuloAssign
.. class:: libcst.MultiplyAssign
.. class:: libcst.PowerAssign
.. class:: libcst.RightShiftAssign
.. autoclass:: libcst.SubtractAssign

In addition, :class:`~libcst.BaseAugOp` is defined purely for typing and isinstance
checks.

.. class:: libcst.BaseAugOp

Miscellaneous
-------------

Miscelaneous nodes that are purely syntactic trivia. While python requires these nodes
in order to parse a module, statement or expression they do not carry any meaning on
their own.

.. autoclass:: libcst.AssignEqual
.. autoclass:: libcst.Colon
.. autoclass:: libcst.Comma
.. autoclass:: libcst.Dot
.. autoclass:: libcst.ImportStar
.. autoclass:: libcst.Semicolon

Whitespace
----------

Nodes that encapsulate pure whitespace.

.. autoclass:: libcst.Comment
.. autoclass:: libcst.EmptyLine
.. autoclass:: libcst.Newline
.. autoclass:: libcst.ParenthesizedWhitespace
.. autoclass:: libcst.SimpleWhitespace
.. autoclass:: libcst.TrailingWhitespace
.. autoclass:: libcst.BaseParenthesizableWhitespace
