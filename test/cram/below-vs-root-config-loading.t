Test the critical difference between --below and --root regarding config
loading.

Set up a workspace with a parent project and a subproject, each with their own
config.

  $ mkdir workspace
  $ cd workspace
  $ touch dune-workspace

Create a parent config that enforces one project name:

  $ cat > dunolint <<EOF
  > (lang dunolint 1.0)
  > 
  > (rule
  >  (enforce (dune_project (name (equals parent_enforced_name)))))
  > EOF

Create a subproject with its own dune-project and config:

  $ mkdir subproject
  $ touch subproject/dune-project

  $ cat > subproject/dunolint <<EOF
  > (lang dunolint 1.0)
  > 
  > (rule
  >  (enforce (dune_project (name (equals subproject_enforced_name)))))
  > EOF

Create a dune-project file in the subproject that will be linted:

  $ cat > subproject/dune-project <<EOF
  > (lang dune 3.17)
  > 
  > (name original_name)
  > EOF

Test 1: Running from subproject WITHOUT any flags. This finds workspace/ as root
and uses parent config.

  $ cd subproject
  $ dunolint lint --dry-run
  Entering directory '$TESTCASE_ROOT/workspace'
  dry-run: Would edit file "subproject/dune-project":
  -1,3 +1,3
    (lang dune 3.17)
    
  -|(name original_name)
  +|(name parent_enforced_name)

Test 2: Running from subproject with --below . This STILL finds workspace/ as
root and uses parent config, but only lints subproject files.

  $ dunolint lint --below . --dry-run
  Entering directory '$TESTCASE_ROOT/workspace'
  dry-run: Would edit file "subproject/dune-project":
  -1,3 +1,3
    (lang dune 3.17)
    
  -|(name original_name)
  +|(name parent_enforced_name)

Test 3: Running from subproject with --root . This forces subproject/ as root
and uses subproject config.

  $ dunolint lint --root . --dry-run
  Entering directory '$TESTCASE_ROOT/workspace/subproject/'
  dry-run: Would edit file "dune-project":
  -1,3 +1,3
    (lang dune 3.17)
    
  -|(name original_name)
  +|(name subproject_enforced_name)

Notice the key differences:
- Tests 1 and 2 both use "parent_enforced_name" because they load the parent config
- Test 3 uses "subproject_enforced_name" because --root prevents traversing to the parent
- The file path also changes: "subproject/dune-project" vs "dune-project"

Test 4: Verify that --root with an absolute path also works correctly:

  $ dunolint lint --root $PWD --dry-run
  dry-run: Would edit file "dune-project":
  -1,3 +1,3
    (lang dune 3.17)
    
  -|(name original_name)
  +|(name subproject_enforced_name)

Test 5: Using --below with an absolute path from the parent directory. This
should use parent config since we're running from the parent.

  $ cd ..
  $ dunolint lint --below $PWD/subproject --dry-run
  dry-run: Would edit file "subproject/dune-project":
  -1,3 +1,3
    (lang dune 3.17)
    
  -|(name original_name)
  +|(name parent_enforced_name)

This test clearly demonstrates that --below and --root can lint the same files
but apply different configurations based on which directory is used as the
workspace root.

