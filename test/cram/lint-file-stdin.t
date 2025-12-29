This test documents the behavior of lint-file when reading from stdin.

Initialize the project root.

  $ ROOT=$(pwd)
  $ touch dune-workspace

When no file argument is provided, lint-file reads from stdin. The --filename
flag is required to determine the file type.

  $ printf '(lang dunolint 1.0)' | dunolint tools lint-file --filename=dunolint
  (lang dunolint 1.0)

  $ printf '(lang dune 3.17)' | dunolint tools lint-file --filename=dune-project
  (lang dune 3.17)

  $ printf '(lang dune 3.17)' | dunolint tools lint-file --filename=dune-workspace
  (lang dune 3.17)

  $ printf '(include_subdirs unqualified)' | dunolint tools lint-file --filename=dune
  (include_subdirs unqualified)

When /dev/stdin is explicitly provided as the file path, the linter rejects it
because it's not within the dune workspace:

  $ printf '(lang dunolint 1.0)' | dunolint tools lint-file /dev/stdin --filename=dunolint
  Error: Path "/dev/stdin" is not in dune workspace.
  [123]

  $ printf '(lang dune 3.17)' | dunolint tools lint-file /dev/stdin --filename=dune-project
  Error: Path "/dev/stdin" is not in dune workspace.
  [123]

The same happens without --filename:

  $ printf '(lang dunolint 1.0)' | dunolint tools lint-file /dev/stdin
  Error: Path "/dev/stdin" is not in dune workspace.
  [123]

This behavior is intentional: when a file path is provided, the linter validates
that the path is within a dune workspace to ensure config discovery works correctly.
When reading from stdin (no file path), this validation is skipped since there's
no meaningful path to validate.
