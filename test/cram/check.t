The dunolint command line tool comes with a util to be used during CI
called `check`. Its goal is to show the parts that are not compliant
and exit with a non-zero code when linting errors are detected.

Initialize the project root.

  $ touch dune-workspace

There is nothing to lint on an empty project.

  $ dunolint lint --check

Let's add some files.

  $ cat > dune-project <<EOF
  > (lang dune 3.17)
  > 
  > (name main)
  > EOF

  $ mkdir -p lib/foo

  $ cat > lib/foo/dune <<EOF
  > (library
  >  (name bar))
  > EOF

We'll pretend there is a _build/ directory, to show that it is ignored by default.

  $ mkdir -p _build/

Let's do some linting!

  $ dunolint lint --check --verbosity=debug
  dunolint: [DEBUG] Visiting directory "./"
  dunolint: [INFO] Loaded dune-project file from "dune-project".
  dunolint: [DEBUG] Config file does not exist at "dunolint".
  dunolint: [INFO] Linting file "dune-project"
  dunolint: [INFO] Linting file "dune-workspace"
  dunolint: [DEBUG] Visiting directory "_build/"
  dunolint: [DEBUG] Dune project file does not exist at "_build/dune-project".
  dunolint: [DEBUG] Config file does not exist at "_build/dunolint".
  dunolint: [INFO] Skipping directory "_build/"
  dunolint: [DEBUG] Visiting directory "lib/"
  dunolint: [DEBUG] Dune project file does not exist at "lib/dune-project".
  dunolint: [DEBUG] Config file does not exist at "lib/dunolint".
  dunolint: [DEBUG] Visiting directory "lib/foo/"
  dunolint: [DEBUG] Dune project file does not exist at "lib/foo/dune-project".
  dunolint: [DEBUG] Config file does not exist at "lib/foo/dunolint".
  dunolint: [INFO] Linting file "lib/foo/dune"

Now let's suppose there are some lints to apply. We'll add manual conditions to
simulate lint errors.

  $ dunolint lint \
  >   --enforce='(dune (library (name (equals foo))))' \
  >   --enforce='(dune_project (name (equals foo)))' \
  >   --check
  check: Would edit file "dune-project":
  -1,3 +1,3
    (lang dune 3.17)
    
  -|(name main)
  +|(name foo)
  
  check: Would edit file "lib/foo/dune":
  -1,2 +1,2
    (library
  -| (name bar))
  +| (name foo))
  
  Error: Linting check failed: Exiting with unaddressed linting errors.
  [123]
