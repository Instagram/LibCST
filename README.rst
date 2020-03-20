.. image:: docs/source/_static/logo/horizontal.svg
   :width: 600 px
   :alt: LibCST

A Concrete Syntax Tree (CST) parser and serializer library for Python

|readthedocs-badge| |circleci-badge| |codecov-badge| |pypi-badge| |pypi-download| |notebook-badge|

.. |readthedocs-badge| image:: https://readthedocs.org/projects/pip/badge/?version=latest&style=flat
   :target: https://libcst.readthedocs.io/en/latest/
   :alt: Documentation

.. |circleci-badge| image:: https://circleci.com/gh/Instagram/LibCST/tree/master.svg?style=shield&circle-token=f89ff46c689cf53116308db295a492d687bf5732
   :target: https://circleci.com/gh/Instagram/LibCST/tree/master
   :alt: CircleCI

.. |codecov-badge| image:: http://codecov.io/gh/Instagram/LibCST/coverage.svg?branch=master
   :target: https://codecov.io/gh/Instagram/LibCST/branch/master
   :alt: CodeCov

.. |pypi-badge| image:: https://img.shields.io/pypi/v/libcst.svg
   :target: https://pypi.org/project/libcst
   :alt: PYPI

.. |pypi-download| image:: https://pepy.tech/badge/libcst/month
   :target: https://pepy.tech/project/libcst/month
   :alt: PYPI Download


.. |notebook-badge| image:: https://img.shields.io/badge/notebook-run-579ACA.svg?logo=data:image/png;base64,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
   :target: https://mybinder.org/v2/gh/Instagram/LibCST/master?filepath=docs%2Fsource%2Ftutorial.ipynb
   :alt: Notebook

.. intro-start

LibCST parses Python 3.0, 3.1, 3.3, 3.5, 3.6, 3.7 or 3.8 source code as a CST tree that keeps
all formatting details (comments, whitespaces, parentheses, etc). It's useful for
building automated refactoring (codemod) applications and linters.

.. intro-end

.. why-libcst-intro-start

LibCST creates a compromise between an Abstract Syntax Tree (AST) and a traditional
Concrete Syntax Tree (CST). By carefully reorganizing and naming node types and
fields, we've created a lossless CST that looks and feels like an AST.

.. why-libcst-intro-end

You can learn more about `the value that LibCST provides 
<https://libcst.readthedocs.io/en/latest/why_libcst.html>`__ and `our 
motivations for the project 
<https://libcst.readthedocs.io/en/latest/motivation.html>`__
in `our documentation <https://libcst.readthedocs.io/en/latest/index.html>`__.
Try it out with `notebook examples <https://mybinder.org/v2/gh/Instagram/LibCST/master?filepath=docs%2Fsource%2Ftutorial.ipynb>`__.

Example expression::

    1 + 2

CST representation::

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

Getting Started
===============

Examining a sample tree
-----------------------

To examine the tree that is parsed from a particular file, do the following::

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

For a more detailed usage example, `see our documentation 
<https://libcst.readthedocs.io/en/latest/tutorial.html>`__.

Installation
------------

LibCST requires Python 3.6+ and can be easily installed using most common Python
packaging tools. We recommend installing the latest stable release from 
`PyPI <https://pypi.org/project/libcst/>`_ with pip:

.. code-block:: shell

    pip install libcst

Further Reading
---------------
- `Static Analysis at Scale: An Instagram Story. <https://instagram-engineering.com/static-analysis-at-scale-an-instagram-story-8f498ab71a0c>`_
- `Refactoring Python with LibCST. <https://chairnerd.seatgeek.com/refactoring-python-with-libcst/>`_

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

We use `isort <https://isort.readthedocs.io/en/stable/>`_ and `black <https://black.readthedocs.io/en/stable/>`_
to format code. To format changes to be conformant, run the following in the root:

.. code-block:: shell

    tox -e autofix

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

We use `Pyre <https://github.com/facebook/pyre-check>`_ for type-checking.

To set up pyre check environment:

1. Copy the example Pyre config: ``cp .pyre_configuration.example .pyre_configuration``.
2. In the config file, add your venv site-packages dir to "search_path". (e.g. add "/workspace/libcst-env/lib/python3.7/site-packages")
3. Remove installed LibCST and install from the source code:

.. code-block:: shell

    pip uninstall -y libcst
    pip install -e .

To verify types for the library, do the following in the root:

.. code-block:: shell

    pyre check

To generate documents, do the following in the root:

.. code-block:: shell

    tox -e docs

Future
======

- Advanced full repository facts providers like fully qualified name and call graph.

License
=======

LibCST is MIT licensed, as found in the LICENSE file.

Acknowledgements
================

- Guido van Rossum for creating the parser generator pgen2 (originally used in lib2to3 and forked into parso).
- David Halter for parso which provides the parser and tokenizer that LibCST sits on top of.
- Zac Hatfield-Dodds for hypothesis integration which continues to help us find bugs.
- Zach Hammer improved type annotation for Mypy compatibility.
