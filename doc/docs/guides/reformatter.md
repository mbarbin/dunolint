# Using `dunolint` as an Emacs reformatter

**dunolint** can be used to automatically reformat dune files during editing with Emacs using the [reformatter](https://github.com/purcell/emacs-reformatter) package. Once the latter is installed, a dunolint reformatter can be configured as such:

```elisp
(reformatter-define dunolint-format
  :program "dunolint"
  :args (list "tools" "lint-file" "--filename" (buffer-file-name))
  :lighter " DoF")
```

