# dune-project

The selector named `(dune_project _)` gives you access to a sub hierarchy of selectors dedicated to stanzas and fields found in *dune-project* files.

In this document you'll find the list of all selectors and predicates that live inside the `(dune_project _)` hierarchy, along with their meaning and some examples.

## generate_opam_files

`(dune_project (generate_opam_files _))` is a selector for the dune-project top level stanza of the same name:

Stanza:
```dune
(generate_opam_files <FRAGMENT>)
```

Its evaluation is *undefined* for all other stanzas. This selector has no predicate, however you can test whether the selector is defined by evaluating `(dune_project (generate_opam_files true))`.

:::warning

Be mindful that `(dune_project (generate_opam_files false))` doesn't test for the absence of that field. Rather this would evaluate to *undefined* if the field isn't present. See the *has_stanza* construct.

:::

## implicit_transitive_deps

`(dune_project (implicit_transitive_deps _))` is a selector for the dune-project top-level stanza of the same name:

Stanza:
```dune
(implicit_transitive_deps <FRAGMENT>)
```

Its evaluation is *undefined* for all other stanzas. Its predicates are:

1. `(equals BOOL)`

Returns *true* iif the BOOL value supplied is an exact match for the value present in the FRAGMENT.

When enforced, dunolint suggests to replace the existing bool value with the one specified by the predicate. Doesn't support suggestion when negated (if you need to, simply go and assert the opposite boolean value).

**Examples:**

Stanza:
```dune
(implicit_transitive_deps true)
```

Condition: `(dune_project (implicit_transitive_deps PREDICATE))`

| Predicate | Result  |
| --------- | ------- |
| (equals true) | True |
| (equals false) | False. Suggestion: sets the field to false |

## name

`(dune_project (name _))` is a selector for the project *name* found at the top level of dune-project files:

Stanza:
```dune
(name <FRAGMENT>)
```

It is almost identical to the `(dune (executable (name _)))` selector, and share the same predicates, thus we are not documenting it in details here.
