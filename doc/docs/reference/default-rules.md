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

## Default Skipped Paths

Dunolint automatically ignores certain directories that typically contain build artifacts, version control metadata, or third-party dependencies. These paths are skipped by default to avoid unnecessary linting and improve performance.

### Skipped Directories

The following directories are ignored by default (anywhere in your project tree, including nested locations):

- `.git/` - Git version control metadata
- `.hg/` - Mercurial version control metadata
- `_build/` - Dune build artifacts
- `_opam/` - Local OPAM switch
- `_coverage/` - Code coverage reports
- `node_modules/` - NPM dependencies
- `doc/build/` - Documentation build artifacts
- `.docusaurus/` - Docusaurus build artifacts
- `*.t/` - Cram test directories

These paths are skipped regardless of whether you have a custom configuration file. You can add your own skip paths using the `(skip_paths ...)` construct in your configuration file (see [Config Language Reference](config/README.md)).

### Future Enhancements

This default list is a baseline to get started. A more robust approach might involve integrating with version control ignore patterns (like `.gitignore` or `.hgignore`), though this presents challenges such as detecting which VCS is in use (if any).

If you have ideas about how dunolint should handle default skip paths, or if you encounter directories that should be added to (or removed from) this default list, please share your feedback in the [discussions](https://github.com/mbarbin/dunolint/discussions) or [issues](https://github.com/mbarbin/dunolint/issues). Your input helps shape the tool's development!

## No Additional Rules by Default

Apart from canonical ordering, dunolint does not enforce any other rules by default. To add more linting rules, you need to create a `dunolint` configuration file.

## Disabling Default Rules

Currently, the canonical ordering rules cannot be disabled. We haven't prioritized making this configurable, as we believe these defaults align well with the mindset of developers interested in a tool like dunolint.

That said, we're open to evolving the configuration capabilities based on real-world needs. If you have a use case where disabling these rules would be valuable, we'd be interested to hear your perspective — please share your thoughts in the [discussions](https://github.com/mbarbin/dunolint/discussions) or [issues](https://github.com/mbarbin/dunolint/issues).

## See Also

- [Config Language Reference](config/README.md) - Learn how to write custom rules
- [Canonical Ordering Philosophy](../explanation/canonical-ordering.md) - Understand the reasoning behind these defaults
- [Quick Start Tutorial](../tutorials/quick-start/README.md) - Get started with custom configurations
