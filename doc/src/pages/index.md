<h1 align="center">
  <p align="center">A Linter for Build Files in Dune Projects (OCaml)</p>
  <img
    src="./img/dunolint.jpg?raw=true"
    width='384'
    alt="Logo"
  />
</h1>

<p align="center">
  <a href="https://github.com/mbarbin/dunolint/actions/workflows/ci.yml"><img src="https://github.com/mbarbin/dunolint/workflows/ci/badge.svg" alt="CI Status"/></a>
  <a href="https://coveralls.io/github/mbarbin/dunolint?branch=main"><img src="https://coveralls.io/repos/github/mbarbin/dunolint/badge.svg?branch=main" alt="Coverage Status"/></a>
  <a href="https://github.com/mbarbin/dunolint/actions/workflows/deploy-doc.yml"><img src="https://github.com/mbarbin/dunolint/workflows/deploy-doc/badge.svg" alt="Deploy Doc Status"/></a>
</p>

Welcome to **dunolint**, a project composed of libraries and a CLI designed to assist with maintaining *build files* typically found in OCaml repos managed by the Dune build system (e.g., files named *dune*, *dune-project*, etc.).

The goal of **dunolint** is to check customizable invariants in your repo and help with ergonomic issues, such as applying systematic changes across many files. It supports things like enabling instrumentation, configuring recurring lint or preprocess flags, sorting libraries alphabetically, and more. You can use it at your convenience during development, and enforce consistency by integrating it into your CI pipeline.
