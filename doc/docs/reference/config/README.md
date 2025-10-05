# Config Language Reference

This document references the constructs and conventions in use in the `dunolint` config files.

If you are looking for a general introduction to the configuration language design and semantics at a higher abstraction level, head over to [this section](../../explanation/config/README.md).

## Config Filename

Dunolint config files are expected to be named `dunolint`. At the moment *dunolint* (the executable) will automatically load a config file named `dunolint` if there is one in the root directory from where the executable is run. We have plans for the linter to load automatically any `dunolint` file found dynamically while walking the tree, however this is not implemented yet. See this [discussion](https://github.com/mbarbin/dunolint/discussions/41). We'll update this section when that feature lands.

## General syntax

A `dunolint` config is made of a sequence of well-formed s-expressions (sexp). It defines a list of stanzas, rules and config values. Its contents kinda look like a `dune` or `dune-project` file, if you will.

### Config Versioning

A `dunolint` file is required to start with an initial stanza that indicates the version of the config language in use in the file. This design is inspired by the similar `(lang dune _)` stanza found in `dune-project` files.

The actual syntax is as follows, for example, using the dunolint config lang version `1.0` below:

```dune
(lang dunolint 1.0)

;; More stanzas below ...
```

As an exception to this rule, a completely *empty* file is accepted as a valid `dunolint` config file, which will be interpreted as being of the latest config version supported by the executable in use. Such empty file will activate the default settings, with no linting rules.

## Complete Example?

As a complementary resource to this reference document, you can find an actual [dunolint](https://github.com/mbarbin/dunolint/blob/main/dunolint) config file at the root of the dunolint repo on GitHub. This file is used by the project's CI to enforce its linting rules to validate contributions made via PRs.

## Stanzas

### Rules

```pre
RULE::
 | (rule EXPR)

EXPR::
 | return
 | (enforce CONDITION)
 | (cond CLAUSES)

CLAUSE::
 | (CONDITION EXPR)
```

**Examples:**

You can look for Dunolint's dunolint configuration that is located in the repository, as well as the repo test suite.

Here is a quick one that enforces the presence of `instrumentation` in all libraries and executables:

```dune
(rule
 (enforce (dune (instrumentation (backend bisect_ppx)))))
```

### Skipping paths

You can configure *dunolint* to ignore files and/or entire directories and skip them. The stanza used for this is `(skip_paths GLOB)`. These stanzas are evaluated in two different contexts while traversing the tree during linting:

1. Before entering any new nested directory;
2. Before linting any file.

The `string` value that is used to evaluate the *GLOB* ends with a `/` character when entering a new directory, and is otherwise the path to the linted file, relative to the location of the dunolint config file containing the skip stanza.

If your intention is to skip an entire directory, then the guideline is to end your *GLOB* with the '/' character.

**Examples:**

```dune
;; Skip that directory and its entire recursive contents.
(skip_paths path/to/my-ignored-dir/)

;; Only ignore that dune file but would include other files found in
;; subdirectories of `foo` if there are any.
(skip_paths path/to/a/file/foo/dune)
```

## Predicates

Written as `(name ARGS)`. Refer to the documentation for each predicate for details.

## Blang

The syntax for blang constructs is: `true`, `false`, `(if _)`, `(or _)`, `(and _)`, and `(not _)`.

*or* and *and* have variable arity and require no extra parenthesis around the list of their arguments.

**Examples:**

- `(not (has_field lint))`
- `(and (stanza library) (has_field preprocess))`

## Conditions

A condition is a blang expression whose leaves are *predicates*.

Conditions may be used to restrict the applicability of a rule, using the rules's `cond` construct. In this case, there are evaluated against the fragments to be linted, as a way to determine whether the rule apply to them (and skip the fragments that are not targeted by the condition).

Conditions may be *enforced* when used in conjunction with the rules's `enforce` construct. In this case, if they are not satisfied *dunolint* will either suggest changes to your files if an automatic modification can be determined, or report the violation asking for manual changes from the user when unable to propose an automatic fix.

**Examples:**

Consider the following rule:

```dune
(rule
 (cond
  ((path (glob vendor/**))
   (enforce (dune (library (not (has_field instrumentation))))))
  (true (enforce (dune (instrumentation (backend bisect_ppx)))))))
```

In this rule, `(path (glob vendor/**))` is a condition testing for the presence under the vendor/ directory. If we are located in that directory, the right-hand-side of the rule will be enforced:

`(dune (library (not (has_field instrumentation))))`

Otherwise, the next `cond` is evaluated. Because the second `cond` is `true`, anything that is *not* in `vendor/**` will enforce `(dune (instrumentation (backend bisect_ppx)))`.

So, effectively what this rule is configured to enforce is that anything under `vendor/**` must have `instrumentation` *disabled*, and everything else, *enabled*, with `bisect_ppx` as instrumentation backend.

## Selectors

Head over to the next pages for selectors dedicated to a certain type of files:

- [dune](./dune.md)
- [dune-project](./dune-project.md)

In addition to them, the following selectors are common to all fragments:

### path

The *path* selector refers to either the path of the directory currently visited by the linter, or the path of the files containing the fragment currently being linted, depending on the context.

#### glob

`(glob GLOB)` defines a predicate whose domain is the selected path, based on the evaluation of the GLOB pattern against the string representation of the path.

When used in a *skip_subtree* rule, it selects the directory visited and in this case the path ends with a trailing `/` character.

##### Examples

> Any file present directly under a directory named *test*, which can be present anywhere in the tree

`(path (glob **/test/*)`

> A few directories commonly ignored at the root of the repo

`(path (or (glob .git/) (glob _build/) (glob _opam/)))`
