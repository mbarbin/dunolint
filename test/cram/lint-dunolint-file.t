This test covers linting dunolint config files.

Initialize the project root.

  $ ROOT=$(pwd)
  $ touch dune-workspace

Test that dunolint config files can be linted and formatted.

  $ printf '(lang dunolint 1.0)\n' > dunolint
  $ dunolint tools lint-file dunolint
  (lang dunolint 1.0)

Test formatting of dunolint config files.

  $ cat > dunolint <<'EOF'
  > (lang
  >  dunolint
  >  1.0)
  > EOF
  $ dunolint tools lint-file dunolint
  (lang dunolint 1.0)

Formatting can be disabled.

  $ cat > dunolint <<'EOF'
  > (lang
  >  dunolint
  >  1.0)
  > EOF
  $ dunolint tools lint-file dunolint --format-file=false
  (lang
   dunolint
   1.0)

Test invalid dunolint version format.

Note: We use printf piping rather than writing to disk for these error tests
because writing an invalid dunolint config to disk would affect config discovery
when the linter runs, changing the code path being tested.

  $ rm dunolint

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
  File "dunolint", line 1, characters 6-10:
  Error: Expected (lang dunolint VERSION) format.
  (lang dune 3.17)
  [123]

Test invalid lang stanza (missing middle constructor).

  $ printf '(lang 1.0)\n' | dunolint tools lint-file --filename=dunolint
  File "dunolint", line 1, characters 0-10:
  Error: Expected (lang dunolint VERSION) format.
  (lang 1.0)
  [123]

Test invalid sexp in dunolint config files.

  $ printf '(invalid sexp\n' | dunolint tools lint-file --filename=dunolint
  File "dunolint", line 2, characters 0-0:
  Error: unclosed parentheses at end of input
  [123]

Test a complete dunolint config file.

  $ cat > dunolint <<EOF
  > (lang dunolint 1.0)
  > 
  > (rule (enforce (dune_project (name (equals foo)))))
  > EOF

  $ dunolint tools lint-file dunolint
  (lang dunolint 1.0)
  
  (rule
   (enforce
    (dune_project
     (name
      (equals foo)))))

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
