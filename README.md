# LibCST

TODO: Add documentation.

# Getting Started

## Examining a sample tree

To examine the tree that is parsed from a particular file, do the following:

```
python -m libcst.tool print <some_py_file.py>
```

# Development

Start by setting up and activating a virtualenv:

```
git clone git@github.com:Instagram/LibCST.git libcst
cd libcst
python3 -m venv ../libcst-env/  # just an example, put this wherever you want
source ../libcst-env/bin/activate
pip install --upgrade pip  # optional, if you have an old system version of pip
pip install -r requirements.txt -r requirements-dev.txt
# If you're done with the virtualenv, you can leave it by running:
deactivate
```

## Auto-formatting code with isort and Black

We use isort and black to format code. To format changes to be conformant, run
the following in the root:

```
isort -q -y && black libcst/
```

## Running tests

To run all tests, do the following in the root:

```
python3 setup.py test
```

## Verifying types with Pyre

To verify types for the library, do the following in the root:

```
pyre check
```

# License

LibCST is MIT licensed, as found in the LICENSE file.

