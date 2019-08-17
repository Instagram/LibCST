Parsing
=======

The parser functions accept source code and an optional configuration object,
and will generate :class:`~libcst.CSTNode` objects.

:func:`~libcst.parse_module` is the most useful function here, since it accepts
the entire contents of a file and returns a new tree, but
:func:`~libcst.parse_expression` and :func:`~libcst.parse_statement` are useful
when inserting new nodes into the tree, because they're easier to use than the
equivalent node constructors.

>>> import libcst as cst
>>> cst.parse_expression("1 + 2")
BinaryOperation(
    left=Integer(
        value='1',
        lpar=[],
        rpar=[],
    ),
    operator=Add(
        whitespace_before=SimpleWhitespace(
            value=' ',
        ),
        whitespace_after=SimpleWhitespace(
            value=' ',
        ),
    ),
    right=Integer(
        value='2',
        lpar=[],
        rpar=[],
    ),
    lpar=[],
    rpar=[],
)


.. autofunction:: libcst.parse_module
.. autofunction:: libcst.parse_expression
.. autofunction:: libcst.parse_statement
.. autoclass:: libcst.PartialParserConfig

Syntax Errors
-------------

.. autoclass:: libcst.ParserSyntaxError
   :members: message, raw_line, raw_column, editor_line, editor_column
   :special-members: __str__
