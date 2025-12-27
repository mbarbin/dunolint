This test covers linting dunolint config files with the `--filename=dunolint`
option and the `(lang dunolint X.Y)` stanza.

Initialize the project root.

  $ ROOT=$(pwd)
  $ touch dune-workspace

Test that dunolint config files can be linted and formatted.

  $ printf '(lang dunolint 1.0)\n' | dunolint tools lint-file --filename=dunolint
  (lang dunolint 1.0)

Test formatting of dunolint config files.

  $ printf '(lang\n dunolint\n 1.0)\n' | dunolint tools lint-file --filename=dunolint
  (lang dunolint 1.0)

Formatting can be disabled.

  $ printf '(lang\n dunolint\n 1.0)\n' \
  > | dunolint tools lint-file --filename=dunolint --format-file=false
  (lang
   dunolint
   1.0)

Test invalid dunolint version format.

  $ printf '(lang dunolint invalid)\n' | dunolint tools lint-file --filename=dunolint
  File "dunolint", line 1, characters 15-22:
  Error: Expected VERSION.MINOR format, got: "invalid".
  (lang dunolint invalid)
  [123]

  $ printf '(lang dunolint 1.INVALID)\n' | dunolint tools lint-file --filename=dunolint
  File "dunolint", line 1, characters 15-24:
  Error: Invalid version format: "1.INVALID".
  (lang dunolint 1.INVALID)
  [123]

Test invalid lang stanza (not dunolint).

  $ printf '(lang dune 3.17)\n' | dunolint tools lint-file --filename=dunolint
  File "dunolint", line 1, characters 0-16:
  Error: Expected (lang dunolint VERSION) format.
  (lang dune 3.17)
  [123]

Test a complete dunolint config file.

  $ cat > dunolint <<EOF
  > (lang dunolint 1.0)
  > 
  > (skip_paths .git/*)
  > 
  > (rule (enforce (dune_project (name (equals foo)))))
  > EOF

  $ dunolint tools lint-file dunolint
  (lang dunolint 1.0)
  
  (skip_paths .git/*)
  
  (rule
   (enforce
    (dune_project
     (name
      (equals foo)))))

Test linting from stdin with relative path override.

  $ cat dunolint | dunolint tools lint-file --filename=path/to/dunolint
  (lang dunolint 1.0)
  
  (skip_paths .git/*)
  
  (rule
   (enforce
    (dune_project
     (name
      (equals foo)))))

Test that absolute paths work when within the workspace.

  $ cat dunolint | dunolint tools lint-file --filename=$PWD/path/to/dunolint
  (lang dunolint 1.0)
  
  (skip_paths .git/*)
  
  (rule
   (enforce
    (dune_project
     (name
      (equals foo)))))

Test that absolute paths outside the workspace are rejected.

  $ cat dunolint | dunolint tools lint-file --filename=/path/to/dunolint
  Error: Path "/path/to/dunolint" is not in dune workspace.
  [123]

Test parse errors in dunolint config files.

  $ printf "(invalid sexp\n" | dunolint tools lint-file --filename=dunolint
  File "dunolint", line 2, characters 0-0:
  Error: unclosed parentheses at end of input
  [123]

Test the --in-place flag with dunolint config files.

Create a directory to hold the test dunolint file (since we already have a
dunolint file in the root).

  $ mkdir -p test-inplace
  $ cat > test-inplace/dunolint <<EOF
  > (lang
  >  dunolint
  >  1.0)
  > EOF

  $ dunolint tools lint-file test-inplace/dunolint --in-place

  $ cat test-inplace/dunolint
  (lang dunolint 1.0)

The command is idempotent.

  $ cp test-inplace/dunolint test-inplace/dunolint-backup

  $ dunolint tools lint-file test-inplace/dunolint --in-place

  $ diff test-inplace/dunolint test-inplace/dunolint-backup

Test that configs are auto-discovered based on --filename path when reading
from stdin. Create a nested directory with a dunolint config:

  $ mkdir -p nested/subdir
  $ cat > nested/dunolint <<EOF
  > (lang dunolint 1.0)
  > (rule (enforce (dune (library (name (equals nested_lib))))))
  > EOF

When reading from stdin with --filename in the nested directory, the nested
config should be loaded and applied to dune files (not dunolint files):

  $ printf '(library (name test))' | dunolint tools lint-file --filename=nested/subdir/dune
  (library
   (name nested_lib))

Test linting a dunolint file from outside the dune workspace.

  $ NO_WORKSPACE=$(mktemp -d)

  $ cat > ${NO_WORKSPACE}/dunolint <<EOF
  > (lang
  >  dunolint
  >  1.0)
  > EOF

  $ (cd ${NO_WORKSPACE} && dunolint tools find-workspace-root)
  Error: I cannot find the root of the current dune workspace/project.
  If you would like to create a new dune project, you can type:
  
      dune init project NAME
  
  Otherwise, please make sure to run dune inside an existing project or
  workspace. For more information about how dune identifies the root of the
  current workspace/project, please refer to
  https://dune.readthedocs.io/en/stable/usage.html#finding-the-root
  [123]

  $ (cd ${NO_WORKSPACE} && dunolint tools lint-file dunolint)
  (lang dunolint 1.0)

  $ rm -rf ${NO_WORKSPACE}

Test that dunolint files in skipped directories are not linted.

  $ mkdir -p .git
  $ cat > .git/dunolint <<EOF
  > (lang
  >  dunolint
  >  1.0)
  > EOF

The `.git/` directory is skipped by default, so the file is not formatted.

  $ cat .git/dunolint | dunolint tools lint-file --filename=.git/dunolint
  (lang
   dunolint
   1.0)

But files in non-skipped directories are formatted.

  $ cat .git/dunolint | dunolint tools lint-file --filename=linted/dunolint
  (lang dunolint 1.0)

Test rule enforcement on dunolint files. Create a root config that enforces
a specific dunolint version, then lint a nested dunolint file with a different
version.

  $ mkdir -p enforce-test
  $ touch enforce-test/dune-workspace
  $ cat > enforce-test/dunolint <<EOF
  > (lang dunolint 1.0)
  > (rule (enforce (dunolint (dunolint_lang_version (= 1.1)))))
  > EOF

Create a nested dunolint file with version 1.0:

  $ mkdir -p enforce-test/nested
  $ cat > enforce-test/nested/dunolint <<EOF
  > (lang dunolint 1.0)
  > EOF

When we lint the nested dunolint file, the root config's rule should enforce
version 1.1:

  $ (cd enforce-test && dunolint tools lint-file nested/dunolint)
  (lang dunolint 1.1)

Test that a dunolint file can enforce rules on itself. The rule applies
during the same lint pass:

  $ mkdir -p self-enforce
  $ touch self-enforce/dune-workspace
  $ cat > self-enforce/dunolint <<EOF
  > (lang dunolint 1.0)
  > (rule (enforce (dunolint (dunolint_lang_version (= 1.1)))))
  > EOF

The rule enforces version 1.1, so the output shows the updated version:

  $ (cd self-enforce && dunolint tools lint-file dunolint)
  (lang dunolint 1.1)
  
  (rule
   (enforce
    (dunolint
     (dunolint_lang_version
      (= 1.1)))))
