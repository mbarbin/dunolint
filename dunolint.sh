#!/bin/bash -e

opam exec dune -- exec dunolint -- lint --config dunolint --check
