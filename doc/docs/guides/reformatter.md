# Using `dunolint` as an Emacs reformatter

**dunolint** can be used to automatically reformat dune files during editing with Emacs using the [reformatter](https://github.com/purcell/emacs-reformatter) package.

## Workspace-aware formatting

When using `dunolint tools lint-file` as an Emacs formatter, dunolint automatically detects the dune workspace that contains the file being formatted. If the file is within a dune workspace (identified by the presence of `dune-workspace` or `dune-project` files in parent directories), dunolint will:

1. Use the workspace root to locate the relevant `dunolint` configuration file(s)
2. Apply the workspace's linting rules and formatting preferences to the file

If the file is not within a dune workspace, dunolint will use the current working directory as the default workspace root. This workspace-aware behavior ensures that your files are always formatted according to your project's configuration, regardless of where you invoke the formatter from.

## Configuration

Once the reformatter package is installed, a dunolint reformatter can be configured as such:

```elisp
(defgroup dunolint-format nil
  "Format dune files using dunolint."
  :group 'languages)

(reformatter-define dunolint-format
  :program "dunolint"
  :args (list "tools" "lint-file" "--filename" (buffer-file-name))
  :lighter " DoF"
  :group 'dunolint-format)
```

To format on save, add:

```elisp
(add-hook 'dune-mode-hook 'dunolint-format-on-save-mode)
```

## Enabling `dune-mode` for dunolint config files

To have dunolint configuration files (`dunolint`) open in `dune-mode` and benefit from format-on-save, add the following to your Emacs configuration:

```elisp
(add-to-list 'auto-mode-alist '("/dunolint\\'" . dune-mode))
```

We have tested this pragmatically and found that the resulting formatting works well on dunolint stanzas, producing a style consistent with other dune files.

Note that this approach relies on the current permissive behavior of `dune format-dune-file`: it can format files whose names (`dunolint`) and stanza types are not part of the dune system itself. This may be brittle if future versions of dune become more strict. If you encounter issues with this setup, please [open an issue](https://github.com/mbarbin/dunolint/issues) so we can discuss improvements or alternative approaches.

### Note on linting self-referencing dunolint config files on save

When linting a dunolint config file that contains rules enforcing changes on itself (e.g., enforcing a specific `dunolint_lang_version`), there is a subtlety with format-on-save workflows:

The reformatter sends the buffer contents to dunolint *before* writing to disk. This means dunolint loads the config from the *previous* saved version of the file, not the current buffer contents. On the first save, dunolint applies the rules from the old config to the buffer contents. If you've just added a new rule to the buffer that would modify the file itself, that rule won't take effect until the second save—by then the file on disk contains the new rule, so it gets applied.
