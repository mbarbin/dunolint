# dunolint

The selector named `(dunolint _)` gives you access to a sub hierarchy of selectors dedicated to stanzas and fields found in *dunolint* config files.

In this document you'll find the list of all selectors and predicates that live inside the `(dunolint _)` hierarchy, along with their meaning and some examples.

## dunolint_lang_version

`(dunolint (dunolint_lang_version _))` is a selector for the dunolint config lang stanza:

Stanza:
```dune
(lang dunolint <VERSION>)
```

For example:
```dune
(lang dunolint 1.0)
```

Its evaluation is *undefined* for all other stanzas.

### Predicates

Predicates take the form `(OP VERSION)` where `OP` is a comparison operator and `VERSION` is a dunolint lang version (e.g., `1.0`). The predicate compares the version found in the stanza against the specified VERSION:

```
<stanza-version> OP <VERSION>
```

For example, with `(lang dunolint 1.0)` in the file, the predicate `(< 1.2)` evaluates as `1.0 < 1.2`, which is *true*.

**Supported operators:**

| S-expression | EDSL | Meaning |
| ------------ | ---- | ------- |
| `=` | `eq` | equal to |
| `>` | `gt` | greater than |
| `>=` | `gte` | greater than or equal to |
| `<` | `lt` | less than |
| `<=` | `lte` | less than or equal to |
| `!=` | `neq` | not equal to |

### Enforcement

When a predicate is enforced, dunolint may suggest updating the version to satisfy the condition:

**Operators with auto-fix:**

- `=`: Sets version to the specified value.
- `>=`: Bumps version up if needed.
- `<=`: Bumps version down if needed.

**Operators without auto-fix:**

- `!=`, `<`, `>`: Enforcement fails if the condition is not already satisfied, letting the user know that a manual edit is required.

### Examples

Given the stanza `(lang dunolint 1.1)`:

| Predicate | Result |
| --------- | ------ |
| `(= 1.1)` | True |
| `(= 1.0)` | False. Suggestion: set version to 1.0 |
| `(!= 1.0)` | True |
| `(!= 1.1)` | False. No suggestion available |
| `(>= 1.0)` | True |
| `(>= 2.0)` | False. Suggestion: set version to 2.0 |
| `(> 1.0)` | True |
| `(> 2.0)` | False. No suggestion available |
| `(<= 1.5)` | True |
| `(<= 1.0)` | False. Suggestion: set version to 1.0 |
| `(< 2.0)` | True |
| `(< 1.0)` | False. No suggestion available |
