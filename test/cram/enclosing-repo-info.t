First we need to setup a repo in a way that satisfies the test environment. This
includes specifics required by the GitHub Actions environment.

  $ ROOT=$(pwd)

  $ (cd / && dunolint lint)
  Error: Failed to locate enclosing repo root from "/".
  [123]

  $ (cd / && dunolint tools enclosing-repo-info)
  Error: Failed to locate enclosing repo root from "/".
  [123]

Git

  $ cd $ROOT

  $ mkdir repo-git

  $ cd repo-git

  $ volgo-vcs init -q .
  $ volgo-vcs set-user-config --user.name "Test User" --user.email "test@example.com"

  $ cat > hello << EOF
  > Hello World
  > EOF

  $ volgo-vcs add hello
  $ rev0=$(volgo-vcs commit -m "Initial commit")

  $ dunolint tools enclosing-repo-info
  ((repo_root
    $TESTCASE_ROOT/repo-git)
   (path_in_repo ./) (vcs_kind git))

  $ mkdir -p path/in/repo

  $ dunolint tools enclosing-repo-info --below path/in/repo
  ((repo_root
    $TESTCASE_ROOT/repo-git)
   (path_in_repo path/in/repo) (vcs_kind git))

  $ (cd path/in/repo ; dunolint tools enclosing-repo-info)
  ((repo_root
    $TESTCASE_ROOT/repo-git)
   (path_in_repo path/in/repo/) (vcs_kind git))

  $ dunolint tools enclosing-repo-info --below ../
  Error: Path
  "$TESTCASE_ROOT/"
  is not in repo.
  [123]

Hg

  $ cd $ROOT

  $ mkdir repo-hg

  $ cd repo-hg

  $ hg init 2> /dev/null

  $ cat > hello << EOF
  > Hello World
  > EOF

  $ volgo-vcs add hello
  $ rev0=$(volgo-vcs commit -m "Initial commit")

  $ dunolint tools enclosing-repo-info
  ((repo_root
    $TESTCASE_ROOT/repo-hg)
   (path_in_repo ./) (vcs_kind hg))

  $ mkdir -p path/in/repo

  $ dunolint tools enclosing-repo-info --below path/in/repo
  ((repo_root
    $TESTCASE_ROOT/repo-hg)
   (path_in_repo path/in/repo) (vcs_kind hg))

  $ (cd path/in/repo ; dunolint tools enclosing-repo-info)
  ((repo_root
    $TESTCASE_ROOT/repo-hg)
   (path_in_repo path/in/repo/) (vcs_kind hg))
