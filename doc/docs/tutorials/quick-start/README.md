# Quick Start

This is your first introduction to dunolint - a tool that helps you maintain consistent build configurations across OCaml projects. If you're familiar with dune but new to dunolint, this tutorial will show you the core concepts in under a minute.

## Simulating an Existing Project

<details>
<summary>
For this mdx tutorial, let's simulate a typical OCaml project with a simple library:
</summary>

For the purpose of this compiled documentation (mdx) we've prepared the contents of a simple dune file under the file `dune.txt` in our repo, we'll copy it as `src/dune` to make the rest of the test use it.

```bash
$ mkdir -p src
$ cat dune.txt > src/dune
```
</details>

```bash
$ cat src/dune
(library
 (name mylib))
```

## Creating Your First Dunolint Configuration

Say you want all libraries & executables to have code coverage instrumentation.

<details>
<summary>
Create a config file named `dunolint`:
</summary>

```bash
$ cat dunolint.txt > dunolint
```
</details>

```bash
$ cat dunolint
(lang dunolint 1.0)

(rule
 (enforce (dune (instrumentation (backend bisect_ppx)))))
```

## Seeing Dunolint in Action

Check what needs to be fixed:

```bash
$ dunolint lint --dry-run
dry-run: Would edit file "src/dune":
-1,2 +1,4
  (library
!| (name mylib)
!| (instrumentation
!|  (backend bisect_ppx)))
```

Apply the fix:

```bash
$ dunolint lint --yes
Editing file "src/dune":
-1,2 +1,4
  (library
!| (name mylib)
!| (instrumentation
!|  (backend bisect_ppx)))
```

That's it! In under a minute, you've seen how dunolint enforces consistent build configurations across your entire project - both existing code and anything you add later.

## Next Steps

- [Install dunolint](../../guides/installation) in your project
- Learn the [configuration language](../../reference/config/README.md)
- Understand dunolint's [design principles](../../explanation/README.md)
