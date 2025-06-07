# dunolint

[![CI Status](https://github.com/mbarbin/dunolint/workflows/ci/badge.svg)](https://github.com/mbarbin/dunolint/actions/workflows/ci.yml)
[![Coverage Status](https://coveralls.io/repos/github/mbarbin/dunolint/badge.svg?branch=main)](https://coveralls.io/github/mbarbin/dunolint?branch=main)
[![Deploy Doc Status](https://github.com/mbarbin/dunolint/workflows/deploy-doc/badge.svg)](https://github.com/mbarbin/dunolint/actions/workflows/deploy-doc.yml)
[![OCaml-CI Build Status](https://img.shields.io/endpoint?url=https://ocaml.ci.dev/badge/mbarbin/dunolint/main&logo=ocaml)](https://ocaml.ci.dev/github/mbarbin/dunolint)

## Introduction

Welcome to **dunolint**, a project composed of libraries and a CLI designed to assist with maintaining *build files* typically found in OCaml repos managed by the Dune build system (e.g., files named *dune*, *dune-project*, etc.).

## Project Goals

The goal of **dunolint** is to check customizable invariants in your repo and help with ergonomic issues, such as applying systematic changes across many files. It supports things like enabling instrumentation, configuring recurring lint or preprocess flags, sorting libraries alphabetically, and more. You can use it at your convenience during development, and enforce consistency by integrating it into your CI pipeline.

## Documentation

Published [here](https://mbarbin.github.io/dunolint).

## Current State

It's currently in the early stages of development and there's little code and features at this point. I'm seeking feedback and early discussions about the project next steps.

## Get Involved

I would love to hear your thoughts about dunolint. If you're interested in this project and would like to engage in discussions or provide feedback, please feel free to open an issue or start a discussion in the GitHub space of the project.

Thank you for your interest in dunolint!

## Acknowledgements

We're very thankful to:
- Jérémie Dimino and the **Dune** developers for their work on the [Dune](https://github.com/ocaml/dune) build system for OCaml.
- The [diataxis](https://diataxis.fr/) approach to technical documentation, which we use as inspiration to structure our doc.
