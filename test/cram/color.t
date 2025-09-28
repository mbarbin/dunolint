In this test we monitor the behavior of color settings.

Initialize the project root.

  $ touch dune-workspace

Let's create some files to lint.

  $ cat > dune <<EOF
  > (library
  >  (name mylib)
  >  (libraries foo bar))
  > EOF

We disable the pager in this test.

  $ export GIT_PAGER=cat

There's something in the way the cram tests are run that makes the ansi char
invisible!

  $ dunolint lint --interactive
  Would edit file "dune":
  -1,3 +1,3
    (library
     (name mylib)
  -| (libraries foo bar))
  +| (libraries bar foo))
  
  [?] Accept diff [N/y/q/?]: 

To actually see them, we make use of `cat -v` below.

We check that it is possible to disable or force the colors when using the
`Git_pager` interface (used in interactive mode).

  $ dunolint lint --interactive --color=always | cat -v
  Would edit file "dune":
  -1,3 +1,3
    (library
     (name mylib)
  ^[[0;1;31m-|^[[0m^[[0m (libraries foo^[[0;31m bar^[[0m))^[[0m
  ^[[0;1;32m+|^[[0m^[[0m (libraries^[[0;32m bar^[[0m foo))^[[0m
  
  [^[[35m?^[[0m] Accept diff [N/y/q/?]: 

  $ dunolint lint --interactive --color=never | cat -v
  Would edit file "dune":
  -1,3 +1,3
    (library
     (name mylib)
  -| (libraries foo bar))
  +| (libraries bar foo))
  
  [?] Accept diff [N/y/q/?]: 

  $ dunolint lint --interactive --color=auto | cat -v
  Would edit file "dune":
  -1,3 +1,3
    (library
     (name mylib)
  -| (libraries foo bar))
  +| (libraries bar foo))
  
  [?] Accept diff [N/y/q/?]: 

We check the same for another mode that doesn't go through the `Git_pager`
interface.

  $ dunolint lint --dry-run --color=always | cat -v
  dry-run: Would edit file "dune":
  -1,3 +1,3
    (library
     (name mylib)
  ^[[0;1;31m-|^[[0m^[[0m (libraries foo^[[0;31m bar^[[0m))^[[0m
  ^[[0;1;32m+|^[[0m^[[0m (libraries^[[0;32m bar^[[0m foo))^[[0m

  $ dunolint lint --dry-run --color=never | cat -v
  dry-run: Would edit file "dune":
  -1,3 +1,3
    (library
     (name mylib)
  -| (libraries foo bar))
  +| (libraries bar foo))

  $ dunolint lint --dry-run --color=auto | cat -v
  dry-run: Would edit file "dune":
  -1,3 +1,3
    (library
     (name mylib)
  -| (libraries foo bar))
  +| (libraries bar foo))
