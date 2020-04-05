.. _libcst-nodes:

Nodes
=====

CSTNode and its subclasses cover Python's full grammar in a
whitespace-sensitive fashion, forming LibCST's concrete syntax tree.

Many of these nodes are designed to `behave similarly to Python's abstract
syntax tree <https://greentreesnakes.readthedocs.io/en/latest/nodes.html>`_.

CSTNode
-------

The base node type which all other nodes derive from.

.. autoclass:: libcst.CSTNode

Module
------

A node that represents an entire python module.

.. autoclass:: libcst.Module

Expressions
-----------

An expression is anything that represents a value (e.g. it could be returned
from a function). All expressions subclass from :class:`~libcst.BaseExpression`.

Expression can be parsed with :func:`~libcst.parse_expression` or as part of a
statement or module using :func:`~libcst.parse_statement` or
:func:`~libcst.parse_module`.

.. autoclass:: libcst.BaseExpression

Names and Object Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. autoclass:: libcst.Name
.. autoclass:: libcst.Attribute

Operations and Comparisons
^^^^^^^^^^^^^^^^^^^^^^^^^^

Operation and Comparison nodes combine one or more expressions with an
operator_.

.. _operator: Operators_

.. autoclass:: libcst.UnaryOperation
.. autoclass:: libcst.BinaryOperation
.. autoclass:: libcst.BooleanOperation
.. autoclass:: libcst.Comparison
.. autoclass:: libcst.ComparisonTarget

Control Flow
^^^^^^^^^^^^

.. autoclass:: libcst.Asynchronous
.. autoclass:: libcst.Await
.. autoclass:: libcst.Yield
.. autoclass:: libcst.From
.. autoclass:: libcst.IfExp

Lambdas and Function Calls
^^^^^^^^^^^^^^^^^^^^^^^^^^

.. autoclass:: libcst.Lambda
.. autoclass:: libcst.Call
.. autoclass:: libcst.Arg

Literal Values
^^^^^^^^^^^^^^

.. autoclass:: libcst.Ellipsis

Numbers
'''''''

.. autoclass:: libcst.BaseNumber
.. autoclass:: libcst.Integer
.. autoclass:: libcst.Float
.. autoclass:: libcst.Imaginary

Strings
'''''''

.. autoclass:: libcst.BaseString
.. autoclass:: libcst.SimpleString
.. autoclass:: libcst.ConcatenatedString

Formatted Strings (f-strings)
'''''''''''''''''''''''''''''

.. autoclass:: libcst.FormattedString
.. autoclass:: libcst.BaseFormattedStringContent
.. autoclass:: libcst.FormattedStringText
.. autoclass:: libcst.FormattedStringExpression

Collections
^^^^^^^^^^^

Simple Collections
''''''''''''''''''

.. autoclass:: libcst.Tuple

.. autoclass:: libcst.BaseList
.. autoclass:: libcst.List

.. autoclass:: libcst.BaseSet
.. autoclass:: libcst.Set

Simple Collection Elements
''''''''''''''''''''''''''

.. autoclass:: libcst.BaseElement
.. autoclass:: libcst.Element
.. autoclass:: libcst.StarredElement

Dictionaries
''''''''''''

.. autoclass:: libcst.BaseDict
.. autoclass:: libcst.Dict

Dictionary Elements
'''''''''''''''''''

.. autoclass:: libcst.BaseDictElement
.. autoclass:: libcst.DictElement
.. autoclass:: libcst.StarredDictElement

Comprehensions
^^^^^^^^^^^^^^

.. autoclass:: libcst.BaseComp
.. autoclass:: libcst.BaseSimpleComp

.. autoclass:: libcst.GeneratorExp
.. autoclass:: libcst.ListComp
.. autoclass:: libcst.SetComp
.. autoclass:: libcst.DictComp

.. autoclass:: libcst.CompFor
.. autoclass:: libcst.CompIf

Subscripts and Slices
^^^^^^^^^^^^^^^^^^^^^

.. autoclass:: libcst.Subscript
.. autoclass:: libcst.BaseSlice
.. autoclass:: libcst.Index
.. autoclass:: libcst.Slice
.. autoclass:: libcst.SubscriptElement

Parenthesis, Brackets, and Braces
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. autoclass:: libcst.LeftParen
.. autoclass:: libcst.RightParen

.. autoclass:: libcst.LeftSquareBracket
.. autoclass:: libcst.RightSquareBracket

.. autoclass:: libcst.LeftCurlyBrace
.. autoclass:: libcst.RightCurlyBrace

Statements
----------

Statements represent a "line of code" or a control structure with other lines of
code, such as an :class:`~libcst.If` block.

All statements subclass from :class:`~libcst.BaseSmallStatement` or
:class:`~libcst.BaseCompoundStatement`.

Statements can be parsed with :func:`~libcst.parse_statement` or as part of a
module using :func:`~libcst.parse_module`.

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
they belong with.

.. Annotation is in the expression module for import-order reasons, it's
   most-often used a a helper for statements (e.g. functions)
.. autoclass:: libcst.Annotation

.. autoclass:: libcst.AsName
.. autoclass:: libcst.AssignTarget
.. autoclass:: libcst.BaseAssignTargetExpression
.. autoclass:: libcst.BaseDelTargetExpression
.. autoclass:: libcst.Decorator
.. autoclass:: libcst.Else
.. autoclass:: libcst.ExceptHandler
.. autoclass:: libcst.Finally
.. autoclass:: libcst.ImportAlias
.. autoclass:: libcst.NameItem

.. Params are in the expression module for import-order reasons, but it makes
   sense to group these closer to FunctionDef than with Lambda.
.. autoclass:: libcst.Parameters
.. autoclass:: libcst.Param
.. autoclass:: libcst.ParamSlash
.. autoclass:: libcst.ParamStar

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

Maybe Sentinel
--------------

.. autoclass:: libcst.MaybeSentinel
