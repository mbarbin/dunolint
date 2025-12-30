# Workspace Root

Dunolint uses the same workspace root detection logic as Dune, ensuring consistent behavior across the toolchain.

## How Dunolint Finds the Workspace Root

When you run `dunolint`, it searches for the workspace root by looking for marker files (`dune-workspace` or `dune-project`) in the current directory and its ancestors.

### Compatibility with Dune

The workspace root detection logic was vendored from Dune 3.20.2, ensuring that both tools agree on what constitutes the project root. For the authoritative description of how Dune finds the workspace root, see the [Dune documentation](https://dune.readthedocs.io/en/stable/usage.html#finding-the-root).

**Note:** We strive to keep this behavior aligned with Dune. If you notice any divergence in how Dunolint and Dune determine the workspace root, please submit an issue so we can realign them.

### The `--root` Flag

You can override the automatic detection by using the `--root=PATH` flag:

<!-- $MDX skip -->
```bash
$ dunolint lint --root=/path/to/project
```

This is the same flag that Dune uses, maintaining consistency between the tools.

### The `DUNE_ROOT` Environment Variable

As an alternative to the `--root` flag, you can set the `DUNE_ROOT` environment variable:

<!-- $MDX skip -->
```bash
$ DUNE_ROOT=/path/to/project dunolint lint
```

This is useful for scripting scenarios or when you want to set the root once for multiple commands. Both absolute paths and paths relative to `cwd` are supported.

**Precedence:** The `--root` flag takes precedence over the `DUNE_ROOT` environment variable.

## Working Directory Change

Once the workspace root is determined, Dunolint changes its working directory to that root before performing any operations. This ensures:

- Consistent path resolution across the project
- Config files are loaded from the expected location
- All relative paths are resolved from the same base

When this directory change occurs (if different from cwd and the log level includes warnings), you'll see a message like:

<!-- $MDX skip -->
```bash
Entering directory '/path/to/workspace/root'
```

## Automatic Config Loading

After changing to the workspace root, Dunolint automatically discovers and loads `dunolint` config files during linting operations. Configs are accumulated from the workspace root down to each linted file's directory, allowing you to have different linting rules for different parts of your project.

For details on how config autoloading works, see [Config Autoloading](./config/autoloading.md).

## Working with Subdirectories: `--below` vs `--root`

### The `--below` Flag

The `--below` flag limits linting to a specific subdirectory while still using the normal workspace root detection:

<!-- $MDX skip -->
```bash
# From within a subdirectory, lint only files in the current directory and below
$ cd src/libs
$ dunolint lint --below .

# From the root, lint only a specific subdirectory
$ dunolint lint --below src/libs
```

When using `--below`, Dunolint:
1. Finds the workspace root normally (walking up to find `dune-workspace` or `dune-project`)
2. Changes to the workspace root
3. **Loads config from the workspace root** (including any `dunolint` file there)
4. Only processes files under the specified path

### Critical Difference: `--below` vs `--root`

While both flags can be used to lint the same set of files, they differ significantly in configuration loading:

- **`--below`**: Finds the true workspace root (potentially a parent directory), loads configs from there, then restricts linting to the specified subdirectory
- **`--root`**: Forces a specific directory to be treated as the workspace root, preventing traversal to parent directories and their configs

#### Example Scenario

Consider this structure:
```text
workspace/
  dunolint           # Config with rule A
  dune-workspace
  subproject/
    dunolint         # Config with rule B
    dune-project
    src/
      file.ml
```

From within `subproject/`:
- `dunolint lint` - Uses `workspace/` as root, applies both rule A and rule B to subproject files
- `dunolint lint --below .` - Uses `workspace/` as root, applies both rule A and rule B to subproject files
- `dunolint lint --root .` - Uses `subproject/` as root, applies only rule B to subproject files

**Important:** Even though `--below .` and `--root .` lint the same files when run from a subproject, they may apply different rules depending on where config files are located in the directory hierarchy. With `--below`, configs accumulate from the workspace root down (both rule A and B apply). With `--root`, only configs from the specified root down are loaded (only rule B applies).

## Path Resolution

When you provide paths as command-line arguments (like `--config` or `--below`), they are resolved as follows:

1. **Relative paths** are resolved relative to your original current directory (before the chdir)
2. **Then** they are made relative to the workspace root (after the chdir)

This two-step process ensures that command-line arguments work intuitively regardless of where you run the command from within your project.
