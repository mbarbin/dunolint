# Using `dunolint` as an Emacs reformatter

**dunolint** can be used to automatically reformat dune files during editing with Emacs using the [reformatter](https://github.com/purcell/emacs-reformatter) package.

## Workspace-aware formatting

When using `dunolint tools lint-file` as am Emacs formatter, dunolint automatically detects the dune workspace that contains the file being formatted. If the file is within a dune workspace (identified by the presence of `dune-workspace` or `dune-project` files in parent directories), dunolint will:

1. Use the workspace root to locate the relevant `dunolint` configuration file(s)
2. Apply the workspace's linting rules and formatting preferences to the file

If the file is not within a dune workspace, dunolint will use the current working directory as the default workspace root. This workspace-aware behavior ensures that your files are always formatted according to your project's configuration, regardless of where you invoke the formatter from.

## Configuration

Once the reformatter package is installed, a dunolint reformatter can be configured as such:

```elisp
(reformatter-define dunolint-format
  :program "dunolint"
  :args (list "tools" "lint-file" "--filename" (buffer-file-name))
  :lighter " DoF")
```

To format on save, add:

```elisp
(add-hook 'dune-mode-hook 'dunolint-format-on-save-mode)
```
