# Explanation

Welcome to the Explanation section of the dunolint documentation. Here, we delve into the details of how dunolint works, its design principles, and our future plans.

## Architecture

The project dunolint is composed of several parts:

- **A CLI tool**, called **dunolint**: It is the main entry point to use the system. It exposes commands to apply linting rules across the files of a project tree, apply custom rules interactively, etc.
- **A linting configuration language**: This language allows you to express *linting rules*, which the CLI tool will load and apply. This is what allows users to customize the linter to the need of their particular code base. The language syntax is based on the *sexp* language, and is intended to be saved to files named *.dunolint*.
- **An OCaml library to write dunolint configs**: Even though dunolint configs can be written as sexps files manually, we do not really encourage this practice. Indeed the dunolint authors are enthusiastic about a practice/pattern sometimes called *config-gen*, where you use the power of your favorite programming language to express and build your config, using the high level types and constructs you fancy, enjoy your regular editor integration, etc. and materialize the concrete config from that automaticallly.
- **A set of OCaml libraries for the linting engine**: They are available for advanced users who wish to leverage some of the existing code. This may come in handy for example when writing complex migration scripts.
- **A dunolint config!**: This is a linting configuration for the dunolint project itself, using dunolint as the linting tool. Although not meant to be user facing, this may serve as an example of a dunolint config used in an actual project.
