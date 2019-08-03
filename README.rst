.. image:: docs/source/_static/logo/horizontal.svg
   :width: 600 px
   :alt: LibCST


.. image:: https://circleci.com/gh/Instagram/LibCST/tree/master.svg?style=svg&circle-token=f89ff46c689cf53116308db295a492d687bf5732
   :target: https://circleci.com/gh/Instagram/LibCST/tree/master
   :alt: CircleCI

.. intro-start

LibCST is a Concrete Syntax Tree (CST) parser and serializer library for Python Code. It parses Python 3.7 source code as a CST tree and keeps all formatting detail (comments, whitespaces, parentheses, etc). It's useful for building Code Modifier (codemod) applications, code formatters, etc.

.. intro-end

.. why-libcst-intro-start
LibCST creates a compromise between an Abstract Syntax Tree (AST) and a traditional Concrete Syntax Tree (CST). By carefully reorganizing and naming node types and fields, we've created a lossless CST that looks and feels like an AST. 

.. why-libcst-intro-end


.. why-libcst-example-start

.. code-block:: python

    fn(1, 2)  # calls fn

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
                                    value=Number(
                                        number=Integer(
                                            value='1',
                                            lpar=[],
                                            rpar=[],
                                        ),
                                        operator=None,
                                        lpar=[],
                                        rpar=[],
                                    ),
                                    keyword=None,
                                    equal=None,
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
                                    value=Number(
                                        number=Integer(
                                            value='2',
                                            lpar=[],
                                            rpar=[],
                                        ),
                                        operator=None,
                                        lpar=[],
                                        rpar=[],
                                    ),
                                    keyword=None,
                                    equal=None,
                                    comma=None,
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
                        semicolon=None,
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

.. why-libcst-example-end

Getting Started
===============

Examining a sample tree
-----------------------

To examine the tree that is parsed from a particular file, do the following:

.. code-block:: shell

    python -m libcst.tool print <some_py_file.py>

Development
-----------

Start by setting up and activating a virtualenv:

.. code-block:: shell

    git clone git@github.com:Instagram/LibCST.git libcst
    cd libcst
    python3 -m venv ../libcst-env/  # just an example, put this wherever you want
    source ../libcst-env/bin/activate
    pip install --upgrade pip  # optional, if you have an old system version of pip
    pip install -r requirements.txt -r requirements-dev.txt
    # If you're done with the virtualenv, you can leave it by running:
    deactivate

We use isort and black to format code. To format changes to be conformant, run
the following in the root:

.. code-block:: shell

    isort -q -y && black libcst/

To run all tests, do the following in the root:

.. code-block:: shell

    tox -e py37

To verify types for the library, do the following in the root:

.. code-block:: shell

    pyre check

To generate documents, do the following in the root:

.. code-block:: shell

    tox -e docs

License
=======

LibCST is MIT licensed, as found in the LICENSE file.

