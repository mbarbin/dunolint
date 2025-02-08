# Config Language Reference

For a general introduction to the configuration language design and semantics, head over to [this section](../../explanation/config/README.md).

## General syntax

A dunolint config is made of a single well-formed s-expression (sexp). It defines a list of rules and config values.

## Predicates

Written as `(name ARGS)`. Refer to the documentation for each predicate for details.

## Blang

The syntax for blang constructs is: `true`, `false`, `(if _)`, `(or _)`, `(and _)`, and `(not _)`.

*or* and *and* have variable arity and require no extra parenthesis around the list of their arguments.

**Examples:**

- `(not (has_field lint))`
- `(and (stanza library) (has_field preprocess))`

## Rules

```pre
Rule(T)::
 | LEAF
 | (cond CLAUSES)

CLAUSE::
 | CONDITION * T

LEAF:
 | (enforce CONDITION)
 | return
 | skip_subtree
```

**Examples:**

You can look for Dunolint's dunolint configuration that is located in the repository, as well as the repo test suite.

## Config

The entire config is a record with the following fields:

**skip_subtree** (optional). A single *rule* which is not allowed to *enforce* any condition, but rather serves to perform *skip_subtree*.

**rules** a list of rules.

```pre
((skip_subtree
   RULE
 )
 (rules (
   RULE
   RULE
   ...
 )))
```

## Selectors

Head over to the next pages for selectors dedicated to a certain type of files. In addition to them, the following selectors are common to all fragments:

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
