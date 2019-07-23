======
LibCST
======
|circleci_badge|

.. |circleci_badge| image:: https://circleci.com/gh/Instagram/LibCST/tree/master.svg?style=svg&circle-token=f89ff46c689cf53116308db295a492d687bf5732
     :target: https://circleci.com/gh/Instagram/LibCST/tree/master
     :alt: CircleCI

LibCST is a Concrete Syntax Tree (CST) parser and serializer library for Python Code. It parses Python sources code as CST tree and keeps all formatting detail (comments, whitespaces, parentheses, etc). It's useful for building Code Modifier (codemod) application, code formatter, etc.

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

