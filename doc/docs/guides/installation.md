# Installation

You can choose to install dunolint from either:

- The public opam-repository (most stable releases)
- A custom opam-repository (more frequent releases)
- From source (most recent version, PRs, etc.)

Releases are currently versioned using the scheme `0.0.YYYYMMDD` while we are in the early stages of development.

For more information about the versioning scheme and the current state of *dunolint*, please see [this page](../explanation/current-state/README.md).

## Release Process

The exact rate at which we publish new versions during the early stages of the project is not really established yet. We'd like to find a balance that feels right between making sure new features and bug fixes are continuously delivered to people experimenting with the project on one hand, but not publishing too many intermediate versions to the public opam repository on the other hand.

The way in which we propose to do this with this project is that we publish somewhat frequent releases of the project to a custom opam-repository. In particular note that not every release that is published that way will make it into a public release - we will skip some, and only publish a public release every so often.

Finally, if you'd like to try or test the latest current version, or the tip of an ongoing *PR*, you may build from source.

## From the Public Opam Repository

Public releases of *dunolint* are published to the main [opam-repository](https://github.com/ocaml/opam-repository). Install it with the [opam](https://opam.ocaml.org) package manager by running:

```sh
opam install dunolint
```

Note that *dunolint* requires to be installed in a switch with `ocaml >= 5.3.0`.

## From the Custom Opam Repository

Intermediate releases for this project are published to a [custom opam-repo](https://github.com/mbarbin/opam-repository.git). To add it to your current opam switch, run:

```sh
opam repo add mbarbin https://github.com/mbarbin/opam-repository.git
```

Then you can install *dunolint* using a normal opam workflow:

```sh
opam install dunolint
```

## Build from sources

Assuming you have a working environment with `OCaml >= 5.3`, `opam` and `dune` you should be able to install the project's dependencies in your current opam switch by running:

```sh
opam install . --deps-only
```

And then build with:

```sh
dune build
```

### Unreleased Packages

From time to time, the project may have temporary dependencies on packages that are being worked on and perhaps not yet published to the main opam repository. When this is the case, these packages are released to the custom opam repository mentioned earlier, and you'll need to add it to the opam switch that you use to build the project.

For example, if you use a local opam switch, this would look like this:

```sh
git clone https://github.com/mbarbin/dunolint.git
cd dunolint
opam switch create . 5.3.0 --no-install
eval $(opam env)
opam repo add mbarbin https://github.com/mbarbin/opam-repository.git
opam install . --deps-only
```

Once this is setup, you can build with dune:

```sh
dune build
```
