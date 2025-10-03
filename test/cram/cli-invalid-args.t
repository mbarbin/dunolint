In this test we cover some invalid usage of the command line tool.

Initialize the project root.

  $ touch dune-workspace

Conflicting flags:

  $ dunolint lint --check --dry-run
  Error: Conflicting flags [dry-run], [check]. Please choose one.
  [124]
