Exercise and monitor cases of finding the root of the current dune workspace.

  $ ROOT=$(pwd)

  $ (cd / && dunolint tools find-workspace-root)
  Error: I cannot find the root of the current dune workspace/project.
  If you would like to create a new dune project, you can type:
  
      dune init project NAME
  
  Otherwise, please make sure to run dune inside an existing project or
  workspace. For more information about how dune identifies the root of the
  current workspace/project, please refer to
  https://dune.readthedocs.io/en/stable/usage.html#finding-the-root
  [123]

  $ cd $ROOT

We should also have code in place that ensure that during the dune tests
environment, we are not using an actual enclosing repo dir as workspace root,
even if that repo contains a `dune-workspace` root file.

  $ dunolint tools find-workspace-root 2>&1 | head -n 1
  Error: I cannot find the root of the current dune workspace/project.

  $ mkdir workspace-foo

  $ cd workspace-foo

  $ touch dune-workspace

  $ dunolint tools find-workspace-root
  $TESTCASE_ROOT/workspace-foo

  $ mkdir project-bar

  $ touch project-bar/dune-project

  $ (cd project-bar && dunolint tools find-workspace-root)
  $TESTCASE_ROOT/workspace-foo

  $ touch project-bar/dune-workspace

  $ (cd project-bar && dunolint tools find-workspace-root)
  $TESTCASE_ROOT/workspace-foo

  $ (cd project-bar && dunolint tools find-workspace-root --root .)
  $TESTCASE_ROOT/workspace-foo/project-bar/

  $ rm dune-workspace

  $ (cd project-bar && dunolint tools find-workspace-root)
  $TESTCASE_ROOT/workspace-foo/project-bar

  $ (cd project-bar && dunolint tools find-workspace-root --root ${ROOT})
  $TESTCASE_ROOT

When entering the workspace-root dunolint produces a message on stderr unless
the log-level does not activate warnings.

  $ (cd project-bar && dunolint lint --root ${ROOT})
  Entering directory '$TESTCASE_ROOT'

  $ (cd project-bar && dunolint lint --root ${ROOT} --log-level=error)
