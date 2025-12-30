Test the DUNE_ROOT environment variable and its interaction with --root.

Setup two separate workspaces:

  $ mkdir workspace-a
  $ touch workspace-a/dune-project

  $ mkdir workspace-b
  $ touch workspace-b/dune-project

Without DUNE_ROOT, auto-detection finds workspace-a:

  $ (cd workspace-a && dunolint tools find-workspace-root)
  $TESTCASE_ROOT/workspace-a

DUNE_ROOT overrides auto-detection to use workspace-b instead:

  $ (cd workspace-a && DUNE_ROOT=../workspace-b dunolint tools find-workspace-root)
  $TESTCASE_ROOT/workspace-b

The --root flag takes precedence over DUNE_ROOT:

  $ (cd workspace-a && DUNE_ROOT=../workspace-b dunolint tools find-workspace-root --root .)
  $TESTCASE_ROOT/workspace-a/

Invalid DUNE_ROOT and --root values are rejected with a user-friendly error:

  $ DUNE_ROOT="" dunolint tools find-workspace-root
  Error: Invalid value for [DUNE_ROOT] environment variable.
  "": invalid path
  [124]

  $ dunolint tools find-workspace-root --root '' 2> output
  [124]

  $ grep '^dunolint: ' output
  dunolint: option '--root': "": invalid path

  $ rm output
