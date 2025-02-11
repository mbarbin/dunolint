# Installation

Dunolint can be installed via the [opam](https://opam.ocaml.org) package manager.

Releases for this project are currently published to a [custom opam-repo](https://github.com/mbarbin/opam-repository.git). To add it to your current opam switch, run:

```sh
opam repo add mbarbin https://github.com/mbarbin/opam-repository.git
```

Then you can install dunolint using a normal opam workflow.

```sh
opam install dunolint
```

## Future plans

We plan to release dunolint to the public opam-repository in the future, and we'll make sure to update this part of the documentation when this is done.

## Build from sources

At this time, the repository still has dependencies to packages only released to the custom opam-repo mentioned earlier, so you'll need to add it to the opam switch that you use to build the project. The linting engine requires OCaml *5.3.0*.

For example, if you use a local opam switch this would look like this:

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
