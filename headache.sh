#!/bin/bash -e

dirs=(
    # Add new directories below:
    "bin"
    "lib/dune_linter/src"
    "lib/dune_linter/test"
    "lib/dune_project_linter/src"
    "lib/dune_project_linter/test"
    "lib/dunolint/src"
    "lib/dunolint/src/dune0"
    "lib/dunolint/src/dune_project0"
    "lib/dunolint/test"
    "lib/dunolint_command/src"
    "lib/dunolint_command/test"
    "lib/dunolint_engine/src"
    "lib/dunolint_engine/test"
    "lib/dunolinter/src"
    "lib/dunolinter/test"
    "dunolint-config/bin"
    "dunolint-config/src"
)

for dir in "${dirs[@]}"; do
    echo "Apply headache to directory ${dir}"

    # Apply headache to .ml files
    headache -c .headache.config -h COPYING.HEADER ${dir}/*.ml

    # Check if .mli files exist in the directory, if so apply headache
    if ls ${dir}/*.mli 1> /dev/null 2>&1; then
        headache -c .headache.config -h COPYING.HEADER ${dir}/*.mli
    fi
done

dune fmt
