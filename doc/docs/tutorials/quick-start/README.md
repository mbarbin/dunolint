# Quick Start

This is your first introduction to dunolint — a tool that helps you maintain
consistent build configurations across OCaml projects. If you're familiar
with dune but new to dunolint, this tutorial will show you the core concepts
in under a minute.

## A Simple Library

Consider a project with a simple library:

`src/dune`:

```dune
(library
 (name mylib))
```

## Creating Your First Dunolint Configuration

Say you want all libraries & executables to have code coverage
instrumentation.

Create a config file named `dunolint`:

`dunolint`:

```dune
(lang dunolint 1.0)

(rule
 (enforce (dune (instrumentation (backend bisect_ppx)))))
```

## Seeing Dunolint in Action

Check what needs to be fixed:

```diff
$ dunolint lint --dry-run
dry-run: Would edit file "src/dune":
@@ -1,2 +1,4 @@
  (library
-| (name mylib))
+| (name mylib)
+| (instrumentation
+|  (backend bisect_ppx)))
```

Apply the fix:

```diff
$ dunolint lint --yes
Editing file "src/dune":
@@ -1,2 +1,4 @@
  (library
-| (name mylib))
+| (name mylib)
+| (instrumentation
+|  (backend bisect_ppx)))
```

That's it! In under a minute, you've seen how dunolint enforces consistent
build configurations across your entire project — both existing code and
anything you add later.

## Next Steps

- [Install dunolint](../../guides/installation) in your project
- Learn the [configuration language](../../reference/config/README.md)
- Understand dunolint's [design principles](../../explanation/README.md)
