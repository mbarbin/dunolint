This test covers linting dune-workspace files.

Initialize the project root.

  $ ROOT=$(pwd)
  $ touch dune-workspace

Test that dune-workspace files can be linted and formatted.

  $ printf '(lang dune 3.17)\n(context\ndefault)\n' > dune-workspace
  $ dunolint tools lint-file dune-workspace
  (lang dune 3.17)
  
  (context default)

Formatting can be disabled.

  $ cat > dune-workspace <<'EOF'
  > (lang
  >  dune
  >  3.17)
  > EOF
  $ dunolint tools lint-file dune-workspace --format-file=false
  (lang
   dune
   3.17)

Test invalid dune version format.

  $ printf '(lang dune invalid)\n' > dune-workspace
  $ dunolint tools lint-file dune-workspace --format-file=false
  File "dune-workspace", line 1, characters 11-18:
  1 | (lang dune invalid)
                 ^^^^^^^
  Error: Expected VERSION.MINOR format, got: "invalid".
  (lang dune invalid)
  [123]

  $ printf '(lang dune 3.INVALID)\n' > dune-workspace
  $ dunolint tools lint-file dune-workspace --format-file=false
  File "dune-workspace", line 1, characters 11-20:
  1 | (lang dune 3.INVALID)
                 ^^^^^^^^^
  Error: Invalid version format: "3.INVALID".
  (lang dune 3.INVALID)
  [123]

Test invalid lang stanza (not dune).

  $ printf '(lang dunolint 1.0)\n' > dune-workspace
  $ dunolint tools lint-file dune-workspace --format-file=false
  File "dune-workspace", line 1, characters 6-14:
  1 | (lang dunolint 1.0)
            ^^^^^^^^
  Error: Expected (lang dune VERSION) format.
  (lang dunolint 1.0)
  [123]

Test invalid lang stanza (missing middle constructor).

  $ printf '(lang 3.17)\n' > dune-workspace
  $ dunolint tools lint-file dune-workspace --format-file=false
  File "dune-workspace", line 1, characters 0-11:
  1 | (lang 3.17)
      ^^^^^^^^^^^
  Error: Expected (lang dune VERSION) format.
  (lang 3.17)
  [123]

Test invalid sexp in dune-workspace files.

  $ printf '(invalid sexp\n' > dune-workspace
  $ dunolint tools lint-file dune-workspace
  File "dune-workspace", line 2, characters 0-0:
  Error: unclosed parentheses at end of input
  [123]

Test a complete dune-workspace file.

  $ cat > dune-workspace <<'EOF'
  > (lang dune 3.17)
  > 
  > (context default)
  > EOF

  $ dunolint tools lint-file dune-workspace
  (lang dune 3.17)
  
  (context default)

Test rule enforcement on dune-workspace files. Create a root config that enforces
a specific dune version, then lint the dune-workspace file with a different
version.

  $ mkdir -p enforce-test
  $ touch enforce-test/dune-workspace
  $ cat > enforce-test/dunolint <<'EOF'
  > (lang dunolint 1.0)
  > (rule (enforce (dune_workspace (dune_lang_version (= 3.19)))))
  > EOF

Create a dune-workspace file with version 3.17:

  $ cat > enforce-test/dune-workspace <<'EOF'
  > (lang dune 3.17)
  > EOF

When we lint the dune-workspace file, the config's rule should enforce
version 3.19:

  $ (cd enforce-test && dunolint tools lint-file dune-workspace)
  (lang dune 3.19)

Test that rules can use >= to enforce minimum version:

  $ mkdir -p min-version-test
  $ touch min-version-test/dune-workspace
  $ cat > min-version-test/dunolint <<'EOF'
  > (lang dunolint 1.0)
  > (rule (enforce (dune_workspace (dune_lang_version (>= 3.17)))))
  > EOF

Create a dune-workspace file with a lower version:

  $ cat > min-version-test/dune-workspace <<'EOF'
  > (lang dune 3.10)
  > EOF

The rule enforces >= 3.17, so the output shows the bumped version:

  $ (cd min-version-test && dunolint tools lint-file dune-workspace)
  (lang dune 3.17)

But if the version is already >= 3.17, it stays unchanged:

  $ cat > min-version-test/dune-workspace <<'EOF'
  > (lang dune 3.19)
  > EOF

  $ (cd min-version-test && dunolint tools lint-file dune-workspace)
  (lang dune 3.19)

Test that path predicates are evaluated when linting dune-workspace files.
Create a config with a path-based rule that only applies to a subdirectory:

  $ mkdir -p path-test/subdir
  $ touch path-test/dune-workspace
  $ cat > path-test/dunolint <<'EOF'
  > (lang dunolint 1.0)
  > (rule
  >  (cond
  >   ((path (glob subdir/*)) (enforce (dune_workspace (dune_lang_version (= 3.20)))))))
  > EOF

Create dune-workspace files in both locations:

  $ cat > path-test/dune-workspace <<'EOF'
  > (lang dune 3.17)
  > EOF

  $ cat > path-test/subdir/dune-workspace <<'EOF'
  > (lang dune 3.17)
  > EOF

The root dune-workspace should not be affected (path doesn't match subdir/*):

  $ (cd path-test && dunolint tools lint-file dune-workspace)
  (lang dune 3.17)

But the subdir dune-workspace matches the path predicate and gets version enforced:

  $ (cd path-test && dunolint tools lint-file subdir/dune-workspace)
  (lang dune 3.20)
