# Config Autoloading

This page explains how dunolint automatically discovers and loads configuration files during linting operations.

## Overview

By default, dunolint automatically discovers and loads `dunolint` config files found in your workspace during the linting process. This allows you to have different linting rules for different parts of your project without needing to explicitly specify config file locations.

## How It Works

### Automatic Discovery

When running `dunolint lint`, the tool:

1. **Starts at the workspace root** (the directory containing `dune-workspace` or `dune-project`)

2. **Searches for `dunolint` config files** as it traverses your project directory tree

3. **Accumulates configs** from the root down to each file being linted

4. **Applies all relevant configs** to each file based on its location in the directory hierarchy

### Config Accumulation

Configs accumulate from root down to the file being linted. For example, given this structure:

<!-- $MDX skip -->
```text
workspace-root/
├── dunolint          (root config)
├── src/
│   ├── dunolint      (src config)
│   └── lib/
│       ├── dunolint  (lib config)
│       └── mylib.ml
```

When linting `src/lib/mylib.ml`, dunolint will apply:
- The root config at `dunolint`
- The src config at `src/dunolint`
- The lib config at `src/lib/dunolint`

All three configs are accumulated and applied together.

### Rule Application Order

Rules from all accumulated configs are applied together. Shallower (parent) configs are processed first, followed by deeper (subdirectory) configs, but the key point is that **all rules apply**.

#### All Rules Are Enforced

When configs accumulate, ALL rules from ALL configs are applied. If you have 3 configs with 5 rules each, all 15 rules will be enforced. Child configs **add** rules; they do not replace or override parent rules.

##### Example: Complementary Rules

A root config requiring instrumentation and a subdirectory config requiring a naming convention:

- Root config: `(enforce (dune (instrumentation (backend bisect_ppx))))`
- Subdirectory config: `(enforce (dune (library (name (is_suffix _internal)))))`
- Result: Both constraints are enforced together on libraries in the subdirectory

##### Example: Conditional Rules

When you need different behavior in specific subtrees, use conditions within a single rule:

```dune
(rule
 (cond
  ((path (glob vendor/**))
   (enforce (dune (library (not (has_field instrumentation))))))
  (true (enforce (dune (instrumentation (backend bisect_ppx)))))))
```

This rule, placed at the root, explicitly states that vendor code should not have instrumentation while everything else should. The logic is self-contained in one place.

## Design Rationale

The autoloading design is motivated by two key principles:

### Compositionality

Dunolint follows the same compositionality model as dune. If you have a standalone repository with a working `dunolint` config at its root, you can seamlessly integrate it as a subdirectory of a larger monorepo that has its own `dunolint` config. The rules from the inner repository continue to work without modification, and the outer repository's rules apply in addition.

This makes it practical to:
- Combine multiple independent projects into a monorepo
- Add project-wide policies at the monorepo root without breaking existing configs
- Maintain modular configurations that work both standalone and integrated

### Self-Contained Rules

Parent rules are intentionally designed to be authoritative. A rule stated in a parent config represents an invariant that applies throughout its subtree. Child configs cannot invalidate or contradict parent rules.

If you read a parent rule, you know it holds everywhere below—there's no risk of discovering later that a child config says otherwise. This makes the configuration predictable and easier to reason about.

When a parent rule genuinely shouldn't apply to certain subdirectories, the parent config should **explicitly exclude** those paths using `skip_paths` or conditional logic:

```dune
;; Parent config explicitly documents the exception
(rule
 (cond
  ((path (glob experimental/**)) return)
  (true (enforce (dune (has_field public_name))))))
```

This approach keeps rules self-contained: anyone reading the parent config understands both the rule and its exceptions without needing to inspect child configs.

## Using `--below` with Autoloading

The `--below` flag limits linting to a specific subdirectory, but **config autoloading still includes ancestor configs**:

<!-- $MDX skip -->
```bash
$ dunolint lint --below src/lib
```

This command will:
1. Load the root `dunolint` config (if it exists)
2. Load `src/dunolint` (if it exists)
3. Load `src/lib/dunolint` (if it exists)
4. Lint only files under `src/lib/`

Even though linting is restricted to `src/lib/`, configs from ancestor directories are still discovered and applied.

## Debug Logging

To see which config files are being loaded, use debug logging:

<!-- $MDX skip -->
```bash
$ dunolint lint --log-level=debug
```

This will show:
- `[DEBUG] Config file does not exist at "path/dunolint"` - when no config is found
- `[INFO] Loaded dunolint config from "path/dunolint"` - when a config is loaded
- `[DEBUG] Visiting directory "path/"` - as directories are traversed

## Best Practices

1. **Start with a root config** - Place a `dunolint` file at your workspace root with project-wide rules

2. **Add subdirectory configs as needed** - Create configs in subdirectories for specialized linting rules

3. **Use deeper configs to complement** - Subdirectory configs augment parent rules to implement directory-specific requirements

4. **Use `skip_paths`** - To completely exclude subdirectories from linting, use `(skip_paths ...)` in a parent config

5. **Test with debug logging** - Use `--log-level=debug` to verify which configs are being loaded for your files

## See Also

- [Config Reference](../../reference/config/README.md) - Complete config file syntax and options
- [Workspace Root](../workspace-root.md) - How dunolint finds the workspace root
