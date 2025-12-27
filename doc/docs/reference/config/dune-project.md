# dune-project

The selector named `(dune_project _)` gives you access to a sub hierarchy of selectors dedicated to stanzas and fields found in *dune-project* files.

In this document you'll find the list of all selectors and predicates that live inside the `(dune_project _)` hierarchy, along with their meaning and some examples.

## dune_lang_version

`(dune_project (dune_lang_version _))` is a selector for the dune-project lang stanza:

Stanza:
```dune
(lang dune <VERSION>)
```

For example:
```dune
(lang dune 3.17)
```

Its evaluation is *undefined* for all other stanzas.

### Predicates

Predicates take the form `(OP VERSION)` where `OP` is a comparison operator and `VERSION` is a dune lang version (e.g., `3.17`). The predicate compares the version found in the stanza against the specified VERSION:

```
<stanza-version> OP <VERSION>
```

For example, with `(lang dune 3.17)` in the file, the predicate `(>= 3.0)` evaluates as `3.17 >= 3.0`, which is *true*.

**Supported operators:**

| Operator | Meaning |
| -------- | ------- |
| `=` | equal to |
| `!=` | not equal to |
| `>=` | greater than or equal to |
| `>` | greater than |
| `<=` | less than or equal to |
| `<` | less than |

### Enforcement

When a predicate is enforced, dunolint may suggest updating the version to satisfy the condition:

- `=` and `!=`: Only `=` supports auto-fix (sets version to the specified value).
- `>=` and `<`: Only `>=` supports auto-fix (bumps version up if needed).
- `<=` and `>`: Only `<=` supports auto-fix (bumps version down if needed).

Operators without auto-fix will fail enforcement if the condition is not already satisfied.

### Examples

Given the stanza `(lang dune 3.17)`:

| Predicate | Result |
| --------- | ------ |
| `(= 3.17)` | True |
| `(= 4.0)` | False. Suggestion: set version to 4.0 |
| `(!= 4.0)` | True |
| `(!= 3.17)` | False. No suggestion available |
| `(>= 3.0)` | True |
| `(>= 4.0)` | False. Suggestion: set version to 4.0 |
| `(> 3.0)` | True |
| `(> 4.0)` | False. No suggestion available |
| `(<= 4.0)` | True |
| `(<= 3.0)` | False. Suggestion: set version to 3.0 |
| `(< 4.0)` | True |
| `(< 3.0)` | False. No suggestion available |

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

1. `(equals VALUE)`

Returns *true* iif the VALUE supplied is an exact match for the value present in the FRAGMENT.

```pre
VALUE := true | false | false-if-hidden-includes-supported
```

When enforced, dunolint suggests to replace the existing fragment's value with the one specified by the predicate. Doesn't support suggestion when negated (if you need to, simply go and enforce the equality with the desired value).

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
