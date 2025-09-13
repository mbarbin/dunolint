# Current State

The project is currently in the early stages of development, and there is little code and features at this point.

## Versioning Tags

Releases are named using the scheme `0.0.YYYYMMDD` while the project is still in its early days. This will allow us to transition to a different scheme later if desired, in accordance with the version comparison function used by *opam*.

## Interfaces and Behavior

During the early stages of development, we are *not* going to version the configuration language or guarantee the stability of interfaces and CLI behavior. This approach allows for more flexible development as we define and refine the project's goals and requirements.

However, we aim to make migrations easy in common cases, keep the tool usable, and document clear migration plans when needed.

## Tests

We are in the process of completing the test suite and aim to achieve 100% code coverage. We are currently prioritizing the areas that are being worked on during the course of other ongoing PRs.

## Migration from Another System

Initially *dunolint* was a private system used by the project's main author with a slightly different architecture. It did not have a eDSL and relied on hard-coded ML files. In the process of generalizing the project for publication, we introduced some instability, which we aim to address as we are completing our test coverage.

## Adoption Hurdles

We prioritize work that removes any adoption hurdles early adopters may encounter, including bug fixes and invalid linting results.

Our goal is to reach a state early in the project's life where motivated users can enable *dunolint* in their projects and find value in it.

Please submit issues on GitHub with any hurdles you encounter.

## Features Prioritization

The current version of *dunolint* is a proof of concept rather than a production-ready product. It has limited features at this point.

Our goal is to achieve substantive coverage for any fields and dune constructs that users find useful.

Beyond dune files, the original system *dunolint* was adapted from also supports basic linting rules for other files commonly found in *dune* projects, such as `.ocamlformat` files. We hope these features will eventually find their place in the *dunolint* project if it makes sense and proves convenient.

Rather than waiting for a complete product before sharing the project, we are iterating on the project incrementally and publicly, following feedback from interested parties.

One area of improvements will be to continue migrating features from the original system to *dunolint*. Note however that this will be limited to the fields used in the codebase the tool was initially created for, and therefore will be quite partial.

You can influence ongoing prioritization by requesting support for the fields and invariants you find most useful. You can do this by opening issues or discussions on the project's GitHub page.
