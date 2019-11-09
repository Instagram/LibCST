===========
Why LibCST?
===========

**Python's ast module already provides a syntax tree. Why do we need another?**

.. include:: ../../README.rst
    :start-after: why-libcst-intro-start
    :end-before: why-libcst-intro-end

Abstract Syntax Trees (AST)
===========================

Let's look at Python's AST for the following code snippet::

    fn(1, 2)  # calls fn

.. container:: toggle

    .. code-block:: python

        ast.Module(
            body=[
                ast.Expr(
                    value=ast.Call(
                        func=ast.Name("fn", ctx=ast.Load()),
                        args=[ast.Num(n=1), ast.Num(n=2)],
                        keywords=[],
                    ),
                ),
            ],
        )

.. graphviz::

    digraph ast {
        layout=dot;
        rankdir=LR;
        splines=polyline;
        ranksep=.6;
        nodesep=.4;
        dpi=300;
        bgcolor=transparent;
        node [
            style=filled,
            color="#fb8d3f",
            fontcolor="#4b4f54",
            fillcolor="#fdd2b3",
            fontname="Source Code Pro Semibold",
            penwidth="2",
        ];
        edge [
            color="#999999",
            fontcolor="#4b4f54",
            fontname="Source Code Pro Semibold",
            fontsize=12,
            penwidth=2,
        ];
        Name [label=" Name('fn') "];
        Load [label=" Load() "];
        Num1 [label=" Num(n=1) "];
        Num2 [label=" Num(n=2) "];
        Module -> Expr [label="body[0]"]
        Expr -> Call [label="value"]
        Call -> Name [label="func"]
        Name -> Load [label="ctx"]
        Call -> Num1 [label="args[0]"]
        Call -> Num2 [label="args[1]"]
    }

This syntax tree does a great job of preserving the semantics of the original code, and the structure of the tree is relatively simple.

However, given only the AST, it wouldn't be possible to reprint the original source code. `Like a JPEG <https://www.youtube.com/watch?v=j5nZhf8SjXw>`_, the Abstract Syntax Tree is lossy.

- The comment we left at the line is gone.
- There's a newline at the end of the file, but the AST doesn't tell us that. It also doesn't tell us if it's ``\n``, ``\r``, or ``\r\n``.
- We've lost some information about the whitespace between the first and second argument.

Abstract Syntax Trees are good for tools like compilers and type checkers where the semantics of code is important, but the exact syntax isn't.


Concrete Syntax Trees (CST)
===========================

A popular CST library for Python is `lib2to3 <https://github.com/python/cpython/tree/master/Lib/lib2to3>`_, which powers tools like `2to3 <https://docs.python.org/3/library/2to3.html>`_ and `Black <https://github.com/ambv/black>`_. Let's look at the syntax tree it generates for the same piece of code::

    fn(1, 2)  # calls fn

.. container:: toggle

    .. code-block:: python
    
        Node(
            file_input,
            children=[
                Node(
                    simple_stmt,
                    children=[
                        Node(
                            power,
                            children=[
                                Leaf(NAME, "fn", prefix=""),
                                Node(
                                    trailer,
                                    children=[
                                        Leaf(LPAR, "(", prefix=""),
                                        Node(
                                            arglist,
                                            children=[
                                                Leaf(NUMBER, "1", prefix=""),
                                                Leaf(COMMA, ",", prefix=""),
                                                Leaf(NUMBER, "2", prefix=" "),
                                            ],
                                        ),
                                        Leaf(RPAR, ")", prefix=""),
                                    ],
                                ),
                            ],
                        ),
                        Leaf(
                            NEWLINE,
                            "\n",
                            prefix="  # calls fn",
                        ),
                    ],
                    prefix=""
                ),
                Leaf(ENDMARKER, "", prefix=""),
            ],
            prefix="",
        )

.. graphviz::

    digraph cst {
        layout=dot;
        rankdir=TB;
        ordering=out;
        splines=line;
        ranksep=.3;
        nodesep=.3;
        dpi=300;
        bgcolor=transparent;
        node [
            style=filled,
            color="#fb8d3f",
            fontcolor="#4b4f54",
            fillcolor="#fdd2b3",
            fontname="Source Code Pro Semibold",
            penwidth="2",
            group=main,
        ];
        edge [
            color="#999999",
            fontcolor="#4b4f54",
            fontname="Source Code Pro Semibold",
            fontsize=12,
            penwidth=2,
        ];
        ENDMARKER [label=" ENDMARKER('') ", color="#3e99ed", fillcolor="#b8d9f8", shape=box];
        NAME_fn [label=" NAME('fn') ", color="#3e99ed", fillcolor="#b8d9f8", shape=box];
        LPAR [label=" LPAR('(') ", color="#3e99ed", fillcolor="#b8d9f8", shape=box];
        NUMBER_1 [label=" NUMBER('1') ", color="#3e99ed", fillcolor="#b8d9f8", shape=box];
        COMMA [label=" COMMA(',') ", color="#3e99ed", fillcolor="#b8d9f8", shape=box];
        NUMBER_2 [label=" NUMBER('2', prefix=' ') ", color="#3e99ed", fillcolor="#b8d9f8", shape=box];
        RPAR [label=" RPAR(')') ", color="#3e99ed", fillcolor="#b8d9f8", shape=box];
        NEWLINE [label="  NEWLINE('\\n', prefix='  # calls fn')  ", color="#3e99ed", fillcolor="#b8d9f8", shape=box];
        file_input -> simple_stmt [label="0"]
        file_input -> ENDMARKER [label="1"]
        simple_stmt -> power [label="0"]
        simple_stmt -> NEWLINE [label="1"]
        power -> NAME_fn [label="0"];
        power -> trailer [label="1"];
        trailer -> LPAR [label="0"];
        trailer -> NUMBER_1 [label="1"];
        trailer -> COMMA [label="2"];
        trailer -> NUMBER_2 [label="3"];
        trailer -> RPAR [label="4"];
    }

This tree is lossless. It retains enough information to reprint the exact input code by storing whitespace information in ``prefix`` properties. This makes it a "Concrete" Syntax Tree, or CST.

However, much of the semantics of the code is now difficult to understand and extract. lib2to3 presents a tree that closely matches `Python's grammar <https://docs.python.org/3/reference/grammar.html>`_ which can be hard to manipulate for complex operations.

- Adding or removing a parameter from ``fn`` requires careful preservation of ``COMMA`` nodes.
- Whitespace and comment ownership is unclear. Deleting nodes could result in invalid generated code.

Concrete Syntax Trees are good for operations that don't significantly change the tree and tools that do not wish to change the semantics of the code itself, such as `Black <https://github.com/ambv/black>`_.

LibCST
======

LibCST takes a compromise between the two formats outlined above. Like a CST, LibCST preserves all whitespace and can be reprinted exactly. Like an AST, LibCST parses source into nodes that represent the semantics of the code.

.. code-block:: python

    fn(1, 2)  # calls fn

.. container:: toggle

    .. code-block:: python

        Module(
            body=[
                SimpleStatementLine(
                    body=[
                        Expr(
                            value=Call(
                                func=Name(
                                    value='fn',
                                    lpar=[],
                                    rpar=[],
                                ),
                                args=[
                                    Arg(
                                        value=Integer(
                                            value='1',
                                            lpar=[],
                                            rpar=[],
                                        ),
                                        keyword=None,
                                        equal=MaybeSentinel.DEFAULT,
                                        comma=Comma(
                                            whitespace_before=SimpleWhitespace(
                                                value='',
                                            ),
                                            whitespace_after=SimpleWhitespace(
                                                value=' ',
                                            ),
                                        ),
                                        star='',
                                        whitespace_after_star=SimpleWhitespace(
                                            value='',
                                        ),
                                        whitespace_after_arg=SimpleWhitespace(
                                            value='',
                                        ),
                                    ),
                                    Arg(
                                        value=Integer(
                                            value='2',
                                            lpar=[],
                                            rpar=[],
                                        ),
                                        keyword=None,
                                        equal=MaybeSentinel.DEFAULT,
                                        comma=MaybeSentinel.DEFAULT,
                                        star='',
                                        whitespace_after_star=SimpleWhitespace(
                                            value='',
                                        ),
                                        whitespace_after_arg=SimpleWhitespace(
                                            value='',
                                        ),
                                    ),
                                ],
                                lpar=[],
                                rpar=[],
                                whitespace_after_func=SimpleWhitespace(
                                    value='',
                                ),
                                whitespace_before_args=SimpleWhitespace(
                                    value='',
                                ),
                            ),
                            semicolon=MaybeSentinel.DEFAULT,
                        ),
                    ],
                    leading_lines=[],
                    trailing_whitespace=TrailingWhitespace(
                        whitespace=SimpleWhitespace(
                            value='  ',
                        ),
                        comment=Comment(
                            value='# calls fn',
                        ),
                        newline=Newline(
                            value=None,
                        ),
                    ),
                ),
            ],
            header=[],
            footer=[],
            encoding='utf-8',
            default_indent='    ',
            default_newline='\n',
            has_trailing_newline=True,
        )

.. graphviz::

    digraph libcst {
        layout=dot;
        rankdir=TB;
        splines=line;
        ranksep=0.5;
        nodesep=1.0;
        dpi=300;
        bgcolor=transparent;
        node [
            style=filled,
            color="#fb8d3f",
            fontcolor="#4b4f54",
            fillcolor="#fdd2b3",
            fontname="Source Code Pro Semibold",
            penwidth="2",
            group=main,
        ];
        edge [
            color="#999999",
            fontcolor="#4b4f54",
            fontname="Source Code Pro Semibold",
            fontsize=12,
            penwidth=2,
        ];
        Module [label="Module"];
        SimpleStatementLine [label="SimpleStatementLine"];
        Expr [label="Expr"];
        Call [label="Call"];
        Name [label="Name"];
        NameValue [label=" 'fn' ", color="#3e99ed", fillcolor="#b8d9f8", shape=box];
        Arg1 [label="Arg"];
        Integer1 [label="Integer"];
        Integer1Value [label=" '1' ", color="#3e99ed", fillcolor="#b8d9f8", shape=box];
        Comma [label="Comma"];
        SimpleWhitespace2 [label="SimpleWhitespace", color="#777777", fillcolor="#eeeeee"];
        SimpleWhitespace2Value [label=" ' ' ", color="#777777", fillcolor="#cccccc", shape=box];
        Arg2 [label="Arg"];
        Integer2 [label="Integer"];
        Integer2Value [label=" '2' ", color="#3e99ed", fillcolor="#b8d9f8", shape=box];
        TrailingWhitespace [label="TrailingWhitespace", color="#777777", fillcolor="#eeeeee"];
        SimpleWhitespace1 [label="SimpleWhitespace", color="#777777", fillcolor="#eeeeee"];
        SimpleWhitespace1Value [label=" '  ' ", color="#777777", fillcolor="#cccccc", shape=box];
        Comment1 [label="Comment", color="#777777", fillcolor="#eeeeee"];
        Comment1Value [label=" '# calls fn' ", color="#777777", fillcolor="#cccccc", shape=box];

        Module -> SimpleStatementLine [label="body[0]"];
        SimpleStatementLine -> Expr [label="body[0]"];
        Expr -> Call [label="value"];
        Call -> Name [label="func"];
        Name -> NameValue [label="value"];
        Call -> Arg1 [label="args[0]"];
        Arg1 -> Integer1 [label="value"];
        Integer1 -> Integer1Value [label="value"];
        Arg1 -> Comma [label="comma"];
        Comma -> SimpleWhitespace2 [label="whitespace_after"];
        SimpleWhitespace2 -> SimpleWhitespace2Value [label="value"];
        Call -> Arg2 [label="args[1]"];
        Arg2 -> Integer2 [label="value"];
        Integer2 -> Integer2Value [label="value"];
        SimpleStatementLine -> TrailingWhitespace [label="trailing_whitespace"];
        TrailingWhitespace -> SimpleWhitespace1 [label="whitespace"];
        SimpleWhitespace1 -> SimpleWhitespace1Value [label="value"];
        TrailingWhitespace -> Comment1 [label="comment"];
        Comment1 -> Comment1Value [label="value"];
    }

LibCST preserves whitespace by parsing it using an internal whitespace parser and assigning it to relevant nodes. This allows for much more granular whitespace ownership and greatly reduces the amount of work necessary to perform complex manipulations. Additionally, it is fully typed. A node's children are well-defined and match the semantics of Python.

However, this does come with some downsides.

- It is more difficult to implement tools that focus almost exclusively on whitespace on top of LibCST instead of lib2to3. For example, `Black <https://github.com/ambv/black>`_ would need to modify whitespace nodes instead of prefix strings, making its implementation much more complex.
- The equivalent AST for a Python module will usually be simpler. We must preserve whitespace ownership by assigning it to nodes that make the most sense which requires us to introduce nodes such as :class:`~libcst.Comma`.
- Parsing with LibCST will always be slower than Python's AST due to the extra work needed to assign whitespace correctly.

Nevertheless, we think that the trade-offs made in LibCST are worthwhile and offer a great deal of flexibility and power.
