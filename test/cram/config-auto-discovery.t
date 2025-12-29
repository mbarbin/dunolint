Test automatic discovery of dunolint config files in subdirectories without root config.

Create a workspace with no root config, only subdirectory configs:

  $ touch dune-workspace
  $ cat > dune-project << EOF
  > (lang dune 3.0)
  > (name test_project)
  > EOF

Create a library at root with no special requirements:

  $ cat > dune << EOF
  > (library
  >  (name root_lib))
  > EOF

Create src/ directory with a config that enforces library names start with "src_":

  $ mkdir -p src
  $ cat > src/dunolint << EOF
  > (lang dunolint 1.0)
  > 
  > (rule
  >  (cond
  >   ((path (glob **)) (enforce (dune (library (name (is_prefix src_))))))))
  > EOF

  $ cat > src/dune << EOF
  > (library
  >  (name src_lib))
  > EOF

Create src/public/ without its own config (inherits from src/):

  $ mkdir -p src/public
  $ cat > src/public/dune << EOF
  > (library
  >  (name src_public_lib))
  > EOF

Create tests/ directory with a config that enforces library names start with "test_":

  $ mkdir -p tests
  $ cat > tests/dunolint << EOF
  > (lang dunolint 1.0)
  > 
  > (rule
  >  (cond
  >   ((path (glob **)) (enforce (dune (library (name (is_prefix test_))))))))
  > EOF

  $ cat > tests/dune << EOF
  > (library
  >  (name test_lib))
  > EOF

Running from root with no root config discovers subdirectory configs:

  $ dunolint lint --dry-run -v 2>&1
  dunolint: [INFO] Linting file "dune"
  dunolint: [INFO] Linting file "dune-project"
  dunolint: [INFO] Linting file "dune-workspace"
  dunolint: [INFO] Loaded dunolint config from "src/dunolint".
  dunolint: [INFO] Linting file "src/dune"
  dunolint: [INFO] Linting file "src/dunolint"
  dunolint: [INFO] Linting file "src/public/dune"
  dunolint: [INFO] Loaded dunolint config from "tests/dunolint".
  dunolint: [INFO] Linting file "tests/dune"
  dunolint: [INFO] Linting file "tests/dunolint"

Linting only src/ directory uses src/dunolint config:

  $ dunolint lint --dry-run --below src -v 2>&1
  dunolint: [INFO] Loaded dunolint config from "src/dunolint".
  dunolint: [INFO] Linting file "src/dune"
  dunolint: [INFO] Linting file "src/dunolint"
  dunolint: [INFO] Linting file "src/public/dune"

Linting only tests/ directory uses tests/dunolint config:

  $ dunolint lint --dry-run --below tests -v 2>&1
  dunolint: [INFO] Loaded dunolint config from "tests/dunolint".
  dunolint: [INFO] Linting file "tests/dune"
  dunolint: [INFO] Linting file "tests/dunolint"

Verify configs accumulate (child configs don't replace parent):

  $ mkdir -p multi/level
  $ cat > multi/dunolint << EOF
  > (lang dunolint 1.0)
  > 
  > (rule
  >  (cond
  >   ((path (glob **)) (enforce (dune (library (has_field name)))))))
  > EOF

  $ cat > multi/level/dunolint << EOF
  > (lang dunolint 1.0)
  > 
  > (rule
  >  (cond
  >   ((path (glob **)) (enforce (dune (library (has_field public_name)))))))
  > EOF

  $ cat > multi/level/dune << EOF
  > (library)
  > EOF

Both configs should apply (name from parent, public_name from local):

  $ dunolint lint --dry-run --below multi 2>&1
  File "multi/level/dune", line 1, characters 0-9:
  1 | (library)
      ^^^^^^^^^
  Error: Enforce Failure.
  The following condition does not hold: (has_field name)
  Dunolint is able to suggest automatic modifications to satisfy linting rules
  when a strategy is implemented, however in this case there is none available.
  Hint: You need to attend and fix manually.
  
  File "multi/level/dune", line 1, characters 0-9:
  1 | (library)
      ^^^^^^^^^
  Error: Enforce Failure.
  The following condition does not hold: (has_field public_name)
  Dunolint is able to suggest automatic modifications to satisfy linting rules
  when a strategy is implemented, however in this case there is none available.
  Hint: You need to attend and fix manually.
  [123]

Invalid config causes directory to be skipped with error:

  $ mkdir -p invalid/subdir
  $ cat > invalid/dunolint << EOF
  > (invalid syntax here)
  > EOF

  $ cat > invalid/dune << EOF
  > (library
  >  (name should_be_skipped))
  > EOF

  $ cat > invalid/subdir/dune << EOF
  > (library
  >  (name also_skipped))
  > EOF

The directory with invalid config should be skipped:

  $ dunolint lint --dry-run 2>&1
  File "invalid/dunolint", line 1, characters 0-21:
  1 | (invalid syntax here)
      ^^^^^^^^^^^^^^^^^^^^^
  Error: Dunolint config expected to start with (lang dunolint VERSION).
  
  File "multi/level/dune", line 1, characters 0-9:
  1 | (library)
      ^^^^^^^^^
  Error: Enforce Failure.
  The following condition does not hold: (has_field name)
  Dunolint is able to suggest automatic modifications to satisfy linting rules
  when a strategy is implemented, however in this case there is none available.
  Hint: You need to attend and fix manually.
  
  File "multi/level/dune", line 1, characters 0-9:
  1 | (library)
      ^^^^^^^^^
  Error: Enforce Failure.
  The following condition does not hold: (has_field public_name)
  Dunolint is able to suggest automatic modifications to satisfy linting rules
  when a strategy is implemented, however in this case there is none available.
  Hint: You need to attend and fix manually.
  [123]

  $ dunolint lint --below invalid --dry-run 2>&1
  File "invalid/dunolint", line 1, characters 0-21:
  1 | (invalid syntax here)
      ^^^^^^^^^^^^^^^^^^^^^
  Error: Dunolint config expected to start with (lang dunolint VERSION).
  [123]

Test that the files in the invalid directory are not linted:

  $ dunolint lint --dry-run -v 2>&1 | grep "Linting file.*invalid"
  [1]
