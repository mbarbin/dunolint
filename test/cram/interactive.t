In this test we exercise the interactive mode.

Initialize the project root.

  $ touch dune-workspace

Create some files to lint.

  $ cat > dune-project <<EOF
  > (name main)
  > EOF

  $ mkdir -p lib/foo

  $ cat > lib/foo/dune <<EOF
  > (library
  >  (name bar))
  > EOF

We'll pretend there is a _build/ directory, to show that it is ignored by default.

  $ mkdir -p _build/

Where this is nothing to lint, the interactive command exits with no prompt.

  $ dunolint lint --interactive --verbose
  dunolint: [INFO] Linting file "dune-project"
  dunolint: [INFO] Skipping directory "_build/"
  dunolint: [INFO] Linting file "lib/foo/dune"

Let's create a config with some rules that are going to apply to the files we
created.

  $ cat > dunolint <<EOF
  > ((rules (
  >   (enforce (dune (library (name (equals foo)))))
  >   (enforce (dune_project (name (equals foo))))
  > )))
  > EOF

We run the lint command in dry-run mode to visualize the changes suggested.

  $ dunolint lint --dry-run
  dry-run: Would edit file "dune-project":
  -1,1 +1,1
  -|(name main)
  +|(name foo)
  
  dry-run: Would edit file "lib/foo/dune":
  -1,2 +1,2
    (library
  -| (name bar))
  +| (name foo))

Note it is possible to restrict the run to a subdirectory only.

  $ dunolint lint --dry-run --below lib/
  dry-run: Would edit file "lib/foo/dune":
  -1,2 +1,2
    (library
  -| (name bar))
  +| (name foo))

Interactive run.

We disable the pager for the test.

  $ export GIT_PAGER=cat

We can quit at any time during the interactive loop.

  $ printf 'q\n' | dunolint lint --interactive
  Would edit file "dune-project":
  -1,1 +1,1
  -|(name main)
  +|(name foo)
  
  [?] Accept diff [N/y/q/?]: 

We can choose to refuse some diff, and accept others.

  $ printf 'n\ny\n' | dunolint lint --interactive
  Would edit file "dune-project":
  -1,1 +1,1
  -|(name main)
  +|(name foo)
  
  [?] Accept diff [N/y/q/?]: 
  Would edit file "lib/foo/dune":
  -1,2 +1,2
    (library
  -| (name bar))
  +| (name foo))
  
  [?] Accept diff [N/y/q/?]: 

  $ cat dune-project
  (name main)

  $ cat lib/foo/dune
  (library
   (name foo))
