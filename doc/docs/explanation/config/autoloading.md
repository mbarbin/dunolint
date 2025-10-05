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

Rules from all accumulated configs are applied in a specific order: shallower (parent) configs are processed first, then deeper (subdirectory) configs.

#### Accumulation vs Precedence

It's important to understand the difference between **accumulation** and **precedence**:

- **Accumulation**: ALL rules from ALL configs are applied. If you have 3 configs with 5 rules each, all 15 rules will be applied.

- **Precedence**: When multiple rules target the same field, the rule applied **last** (from the deepest config) has the final say.

#### Example: All Rules Apply

If your root config enforces `(name (is_prefix foo))` and your subdirectory config enforces `(name (is_prefix bar))`:

- **Both rules apply** in sequence
- Root rule transforms `mylib` → `foomylib`
- Subdirectory rule then transforms `foomylib` → `barfoomylib`
- Final result: `barfoomylib`

The subdirectory rule "wins" because it was applied last, but the root rule still affected the result. This is accumulation with precedence.

#### Example: Different Fields

When rules target **different fields**, all rules are applied successfully:
- Root config: `(has_field public_name)`
- Subdirectory config: `(name (equals specific_name))`
- Result: Both constraints are enforced

#### Design Rationale

This design allows for specialization: you can set general rules at the root and add or override them with more specific rules in subdirectories as needed.

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

3. **Use deeper configs to specialize** - Subdirectory configs can override parent rules to implement directory-specific requirements

4. **Use `skip_paths`** - To completely exclude subdirectories from linting, use `(skip_paths ...)` in a parent config

5. **Test with debug logging** - Use `--log-level=debug` to verify which configs are being loaded for your files

## See Also

- [Config Reference](../../reference/config/README.md) - Complete config file syntax and options
- [Workspace Root](../workspace-root.md) - How dunolint finds the workspace root
