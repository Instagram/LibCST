===========
Why LibCST?
===========

**Python's ast module already provides a syntax tree. Why do we need another?**

.. include:: ../../README.rst
    :start-after: why-libcst-intro-start
    :end-before: why-libcst-intro-end

Abstract Syntax Trees (AST)
===========================

Let's look at Python's AST for the following code snippet:

.. code-block:: python

    fn(1, 2)  # calls fn

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
- There's a newline at the end of the file, but the AST doesn't tell us that. It also doesn't tell us if it's `\\n`, `\\r`, or `\\r\\n`.
- We've lost some information about the whitespace between the first and second argument.

Abstract Syntax Trees are good for tools like compilers and type checkers where the semantics of code is important, but the exact syntax isn't.


Concrete Syntax Trees (CST)
===========================

A popular CST library for Python is `lib2to3 <https://github.com/python/cpython/tree/master/Lib/lib2to3>`_, which powers tools like `2to3 <https://docs.python.org/3/library/2to3.html>`_ and `Black <https://github.com/ambv/black>`_. Let's look at the syntax tree it generates for the same piece of code:

.. code-block:: python

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

This tree is lossless. It retains enough information to reprint the exact input code by storing whitespace information in `prefix` properties. This makes it a "Concrete" Syntax Tree, or CST.

LibCST
======

.. include:: ../../README.rst
    :start-after: why-libcst-example-start
    :end-before: why-libcst-example-end

