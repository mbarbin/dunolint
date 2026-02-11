In this test we exercise a few edge cases related to auto formatting dune files,
with a focus on characterizing what dune lang version we choose to apply.

Initialize the project root.

  $ ROOT=$(pwd)
  $ touch dune-workspace

Let's start with [lint-file].

When there is no enclosing dune-project file, the contents is auto-formatted
with a default version.

  $ printf '(lang dune 3.17)\n(name\nmy_project)\n' | dunolint tools lint-file --filename=dune-project
  (lang dune 3.17)
  
  (name my_project)

If the dune-project file is present but is invalid, no formatting will apply.

  $ cat > dune-project <<EOF
  > (invalid
  > EOF

  $ printf '(lang dune 3.17)\n(name\nmy_project)\n' | dunolint tools lint-file --filename=dune-project
  (lang dune 3.17)
  (name
  my_project)

We can get more information for investigating what is happening by enabling a
more verbose log level.

  $ printf '(lang dune 3.17)\n(name\nmy_project)\n' | dunolint tools lint-file --filename=dune-project --log-level=info
  File "dune-project", line 2, characters 0-0:
  Info: unclosed parentheses at end of input
  (lang dune 3.17)
  (name
  my_project)

  $ printf '(lang dune 3.17)\n(name\nmy_project)\n' | dunolint tools lint-file --filename=dune-project --log-level=debug
  File "dune-project", line 2, characters 0-0:
  Info: unclosed parentheses at end of input
  dunolint: [DEBUG] Config file does not exist at "dunolint".
  
  File "dune-project", line 1, characters 0-0:
  Debug: Formatting file "dune-project" was disabled due to existing errors in
  enclosing "dune-project" file.
  (lang dune 3.17)
  (name
  my_project)

An edge case is if the dune-project file exists and can be parsed by dunolint,
but is missing its dune lang stanza. In this case we rely on dune to complain,
and apply no formatting (we do not use a default version).

  $ cat > dune-project <<EOF
  > (name project)
  > EOF

  $ printf '(lang dune 3.17)\n(name\nmy_project)\n' | dunolint tools lint-file --filename=dune-project --log-level=debug
  dunolint: [INFO] Loaded dune-project file from "dune-project".
  dunolint: [DEBUG] Config file does not exist at "dunolint".
  (lang dune 3.17)
  (name
  my_project)
