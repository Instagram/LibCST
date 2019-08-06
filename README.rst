.. image:: docs/source/_static/logo/horizontal.svg
   :width: 600 px
   :alt: LibCST

.. image:: https://circleci.com/gh/Instagram/LibCST/tree/master.svg?style=svg&circle-token=f89ff46c689cf53116308db295a492d687bf5732
   :target: https://circleci.com/gh/Instagram/LibCST/tree/master
   :alt: CircleCI

.. intro-start

LibCST is a Concrete Syntax Tree (CST) parser and serializer library for Python Code.
It parses Python 3.6 or Python 3.7 source code as a CST tree and keeps all formatting
detail (comments, whitespaces, parentheses, etc). It's useful for building automated
refactoring (codemod) applications, code formatters, linters, etc.

.. intro-end

.. why-libcst-intro-start
LibCST creates a compromise between an Abstract Syntax Tree (AST) and a traditional
Concrete Syntax Tree (CST). By carefully reorganizing and naming node types and
fields, we've created a lossless CST that looks and feels like an AST.

.. why-libcst-intro-end


.. why-libcst-example-start

::

    1 + 2

::

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

.. why-libcst-example-end

Getting Started
===============

Examining a sample tree
-----------------------

To examine the tree that is parsed from a particular file, do the following:

::

    python -m libcst.tool print <some_py_file.py>

Alternatively you can import LibCST into a Python REPL and use the included parser
and pretty printing functions:

>>> import libcst as cst
>>> from libcst.tool import dump
>>> print(dump(cst.parse_expression("(1 + 2)")))
BinaryOperation(
  left=Integer(
    value='1',
  ),
  operator=Add(),
  right=Integer(
    value='2',
  ),
  lpar=[
    LeftParen(),
  ],
  rpar=[
    RightParen(),
  ],
)

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

We use `isort <https://github.com/timothycrosley/isort>`+_ and `black <https://github.com/psf/black>`_
to format code. To format changes to be conformant, run the following in the root:

.. code-block:: shell

    isort -q -y && black libcst/

To run all tests, you'll need to install `tox <https://tox.readthedocs.io/en/latest/>`_
and do the following in the root:

.. code-block:: shell

    tox -e py37

You can also run individual tests by using unittest and specifying a module like
this:

.. code-block:: shell

    python -m unitttest libcst.tests.test_batched_visitor

See the `unittest documentation <https://docs.python.org/3/library/unittest.html>`_
for more examples of how to run tests.

We use `Pyre <https://github.com/facebook/pyre-check>`_ for type-checking. To
verify types for the library, do the following in the root:

.. code-block:: shell

    pyre check

To generate documents, do the following in the root:

.. code-block:: shell

    tox -e docs

License
=======

LibCST is MIT licensed, as found in the LICENSE file.
