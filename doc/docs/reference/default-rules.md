# Default Rules

This page documents the linting rules that dunolint applies by default when no configuration is specified.

## Canonical Field Ordering

Without any configuration, dunolint enforces a canonical ordering for various fields in your dune files. This means that certain lists and fields are automatically sorted according to predefined rules.

### What Gets Sorted

#### Library Dependencies
In `(libraries ...)` stanzas, dependencies are sorted alphabetically:

```dune
;; Before
(libraries baz foo bar qux)

;; After
(libraries bar baz foo qux)
```

#### PPX Preprocessors
In `(preprocess (pps ...))` stanzas, PPX rewriters are sorted alphabetically:

```dune
;; Before
(preprocess (pps ppx_jane ppx_deriving.std ppx_assert))

;; After
(preprocess (pps ppx_assert ppx_deriving.std ppx_jane))
```

### Future Scope

We've focused on fields that have been most impactful in our experience, but we believe there are more opportunities to apply canonical ordering principles. We plan to expand the scope of fields covered by these rules in future versions of dunolint. The specific fields included will evolve with the configuration language and tool versions.

### Comment Handling

Comments are preserved and moved along with their associated items. See [Comments in Libraries](../explanation/linting-equilibrium/comments-in-libraries.md) for detailed behavior.

## No Additional Rules by Default

Apart from canonical ordering, dunolint does not enforce any other rules by default. To add more linting rules, you need to create a `dunolint` configuration file.

## Disabling Default Rules

Currently, the canonical ordering rules cannot be disabled. We haven't prioritized making this configurable, as we believe these defaults align well with the mindset of developers interested in a tool like dunolint.

That said, we're open to evolving the configuration capabilities based on real-world needs. If you have a use case where disabling these rules would be valuable, we'd be interested to hear your perspective — please share your thoughts in the [discussions](https://github.com/mbarbin/dunolint/discussions) or [issues](https://github.com/mbarbin/dunolint/issues).

## See Also

- [Config Language Reference](config/README.md) - Learn how to write custom rules
- [Canonical Ordering Philosophy](../explanation/canonical-ordering.md) - Understand the reasoning behind these defaults
- [Quick Start Tutorial](../tutorials/quick-start/README.md) - Get started with custom configurations
