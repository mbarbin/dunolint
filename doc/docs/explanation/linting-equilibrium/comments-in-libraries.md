# Comments in `libraries`

For the purpose of this compiled documentation (mdx) we'll make it so that we are placed in a dedicated workspace for the duration of the test.

```sh
$ touch dune-workspace
```

The `libraries` dependencies listed in `library` stanzas are sorted alphabetically. For example, this dune file:

```sh
$ cat > dune0 <<EOF\
> (library\
>  (name my_lib)\
>  (libraries foo bar baz))\
> EOF
```

is linted as follows:

```sh
$ dunolint tools lint-file dune0 --filename=dune
(library
 (name my_lib)
 (libraries bar baz foo))
```

The process of reordering the libraries items becomes more complex in the presence of comments. This section illustrates how comments are handled in this field, discusses limitations, and suggests conventions to satisfy the linter in certain unsupported cases.

## End-of-line Comments

When a comment is placed next to an entry, the tool assumes that it is attached to that item and moves it along with the item during reordering.

```sh
$ cat > dune0 <<EOF\
> (library\
>  (name my_lib)\
>  (libraries\
>   aa\
>   dd ;; this a comment\
>   zz\
>   bb\
>   cc))\
> EOF
```

```sh
$ dunolint tools lint-file dune0 --filename=dune
(library
 (name my_lib)
 (libraries
  aa
  bb
  cc
  dd ;; this a comment
  zz))
```

So far, so good.

## Sections Comments

When a comment is placed on its own line, it is less clear whether it applies to the following line only or to multiple lines. The reordering implemented in *dunolint* assumes that comments on their own lines are *section delimiters*. Libraries within each section are reordered, but no reordering occurs between or across sections:

```sh
$ cat > dune0 <<EOF\
> (library\
>  (name my_lib)\
>  (libraries\
>   ;; First section\
>   jj\
>   ii\
>   ;; Section section\
>   bb\
>   aa\
>   ;; Third section\
>   dd\
>   cc))\
> EOF
```

```sh
$ dunolint tools lint-file dune0 --filename=dune
(library
 (name my_lib)
 (libraries
  ;; First section
  ii
  jj
  ;; Section section
  aa
  bb
  ;; Third section
  cc
  dd))
```

### Suggested Workaround

This behavior may not produce the desired result when a comment is intended to apply only to the immediate subsequent line. For example:

```sh
$ cat > dune0 <<EOF\
> (library\
>  (name my_lib)\
>  (libraries\
>   ;; foo needs to be first.\
>   foo\
>   baz\
>   bar))\
> EOF
```

The linter processes this as follows:

```sh
$ dunolint tools lint-file dune0 --filename=dune
(library
 (name my_lib)
 (libraries
  ;; foo needs to be first.
  bar
  baz
  foo))
```

To achieve the intended result, we recommend converting the comment into a section comment:

```sh
$ cat > dune0 <<EOF\
> (library\
>  (name my_lib)\
>  (libraries\
>   ;; foo needs to be first.\
>   foo\
>   ;; Other libs may be ordered as usual.\
>   baz\
>   bar))\
> EOF
```

The linter preserves the sections, ensuring the comment's scope is clear. This approach not only satisfies the linter but also makes the intent of the comment clearer to human readers:

```sh
$ dunolint tools lint-file dune0 --filename=dune
(library
 (name my_lib)
 (libraries
  ;; foo needs to be first.
  foo
  ;; Other libs may be ordered as usual.
  bar
  baz))
```

By adopting this convention, you can maintain clarity and consistency in your code while working harmoniously with the linter. This approach helps you achieve a *linting equilibrium* — a balance where your code reflects both your intent and the linter's requirements, resulting in a more maintainable and readable codebase.
