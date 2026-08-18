+++
title = "A Linter for Build Files in Dune Projects (OCaml)"
template = "index.html"
sort_by = "weight"
+++

Welcome to **dunolint**, a project composed of libraries and a CLI designed
to assist with maintaining *build files* typically found in OCaml repos
managed by the Dune build system (e.g., files named *dune*, *dune-project*,
etc.).

The goal of **dunolint** is to check customizable invariants in your repo
and help with ergonomic issues, such as applying systematic changes across
many files. It supports things like enabling instrumentation, configuring
recurring lint or preprocess flags, sorting libraries alphabetically, and
more. You can use it at your convenience during development, and enforce
consistency by integrating it into your CI pipeline.
