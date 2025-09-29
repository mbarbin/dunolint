# First Run Through

This tutorial walks you through running dunolint on an existing OCaml codebase to see what it would change with its default configuration.

## What You'll Learn

- How to run dunolint on your project
- How to interpret the suggested changes
- Different workflows for adopting (or not adopting) dunolint

## Prerequisites

You'll need:
- An OCaml project that uses dune
- dunolint installed (see [installation guide](../../guides/installation))

## Your First Lint

Navigate to the root of your OCaml project and run:

<!-- $MDX skip -->
```sh
$ dunolint lint
```

By default, in a terminal, dunolint runs in **interactive mode**. For each suggested change, you'll see a diff and a prompt:

<!-- $MDX skip -->
```diff
Editing file "lib/mylib/dune":
-1,5 +1,5
  (library
   (name mylib)
   (libraries
-   foo
-   bar
+   bar
+   foo
   )
  )
Apply this change? (y/N):
```

Your options:
- `y` - apply this change
- `n` or Enter - skip this change
- `q` - quit

## Seeing All Changes at Once

To see all proposed changes without applying them:

<!-- $MDX skip -->
```sh
$ dunolint lint --dry-run
```

This shows what would be changed without modifying any files:

<!-- $MDX skip -->
```diff
dry-run: Would edit file "lib/mylib/dune":
-1,5 +1,5
  (library
   (name mylib)
   (libraries
-   foo
-   bar
+   bar
+   foo
   )
  )

dry-run: Would edit file "bin/dune":
-1,3 +1,3
  (executable
   (name main)
-  (libraries utils mylib)
+  (libraries mylib utils)
  )

Found 2 files that would be modified.
```

## What's Being Changed?

By default, dunolint enforces [canonical ordering](../../reference/default-rules.md) — alphabetically sorting various fields like library dependencies and PPX preprocessors. This is the only thing it does without configuration.

If you're curious about why these defaults exist, see the [philosophy behind canonical ordering](../../explanation/canonical-ordering.md).

## Interpreting the Results

After your first run, you'll see one of these patterns:

**Few changes**: Your codebase is already quite consistent. You might apply them all at once or gradually.

**Many changes**: Common for projects that haven't used an enforcement tool. This is normal.

**Too many changes**: If the changes feel overwhelming or incompatible with your project's conventions, dunolint might not be the right fit, and that's okay.

**Something looks wrong?** If you see a suspicious change or something that doesn't look right, please [open an issue](https://github.com/mbarbin/dunolint/issues) with a link to your file, the repository revision, and your `dunolint --version` output. We appreciate bug reports!

## Possible Next Steps

### Apply and Move On
If you like what you see:
1. Run `dunolint lint --yes` to apply all changes
2. Commit them in a "normalize dune files" PR
3. Continue development as before

### Gradual Adoption
If you want to ease into it:
1. Apply changes file by file as you work on different parts
2. Run `dunolint lint --below=some/path` to lint specific directories
3. Get comfortable before adding to CI

### Regular Cleanup
Even without CI enforcement:
1. Run `dunolint lint --dry-run` before releases
2. Apply changes in dedicated cleanup PRs
3. Keep things tidy without strict enforcement

### Full Enforcement
Ready to automate:
1. Apply all current changes
2. Add `dunolint lint --check` to your CI pipeline
3. Never think about field ordering again

For GitHub Actions users, there's a [dunolint-actions](https://github.com/mbarbin/dunolint-actions) repository that provides reusable workflows. These actions use pre-built binaries and typically run in 10-20 seconds. This is an area of active development, and we're planning a dedicated CI tutorial in the future.

### The Ultimate: Format on Save
Some developers go even further and configure their editor to run `dunolint lint` on every save. Yes, you'll see fields reordering as you save — this might be too extreme for some, but others love the immediate feedback. Currently, this is only supported in Emacs (see [setup guide](../../guides/reformatter.md)). Support for other editors would be a welcome contribution if there's interest!

### Custom Configuration
Want different rules:
1. Create a `dunolint` file in your project root
2. Define your own rules (see [config reference](../../reference/config/README.md))
3. Go beyond the defaults

### Working with Build Constraints
In rare cases, library dependencies must be in a specific order due to build constraints. If you have documented this with comments, dunolint respects section boundaries. See [Comments in Libraries](../../explanation/linting-equilibrium/comments-in-libraries.md) for how to structure your dependencies to work with the linter while preserving required ordering.

## Not for Everyone

Dunolint enforces opinions about build file conventions and configuration. If you prefer manual control over these details, or if your team has different conventions, that's completely valid. Not every tool fits every project.

## Next Steps

- [Default Rules Reference](../../reference/default-rules.md) - What dunolint changes by default
- [Configuration Language](../../reference/config/README.md) - Write custom rules
- [Quick Start Tutorial](../quick-start/README.md) - Create your first configuration
