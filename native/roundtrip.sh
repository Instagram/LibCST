#!/bin/bash

PARSE=$(dirname $0)/target/release/parse

exec diff -u "$1" <($PARSE < "$1")
