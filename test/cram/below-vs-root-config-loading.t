Test config autoloading and how --root, --below, and --config flags affect it.

Set up a workspace with a parent project and a subproject, each with their own
config to demonstrate config accumulation and precedence.

  $ mkdir workspace
  $ cd workspace
  $ touch dune-workspace

Create a parent config at workspace root:

  $ cat > dunolint <<EOF
  > (lang dunolint 1.0)
  > 
  > (rule
  >  (enforce (dune_project (name (equals root_enforced_name)))))
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

Running from subproject WITHOUT any flags. With config autoloading, both the
root config and subproject config are discovered and applied. The subproject
config is applied last, so it takes precedence.

  $ cd subproject
  $ dunolint lint --dry-run
  Entering directory '$TESTCASE_ROOT/workspace'
  dry-run: Would edit file "subproject/dune-project":
  -1,3 +1,3
    (lang dune 3.17)
    
  -|(name original_name)
  +|(name subproject_enforced_name)

Running from subproject with --below . Same behavior: configs are auto-loaded
from workspace root down to the subproject. The --below flag only limits which
files are linted, not which configs are discovered.

  $ dunolint lint --below . --dry-run
  Entering directory '$TESTCASE_ROOT/workspace'
  dry-run: Would edit file "subproject/dune-project":
  -1,3 +1,3
    (lang dune 3.17)
    
  -|(name original_name)
  +|(name subproject_enforced_name)

Running from subproject with --root . This forces subproject/ as workspace root,
so only the subproject config is discovered (parent config is outside the
workspace).

  $ dunolint lint --root . --dry-run
  Entering directory '$TESTCASE_ROOT/workspace/subproject/'
  dry-run: Would edit file "dune-project":
  -1,3 +1,3
    (lang dune 3.17)
    
  -|(name original_name)
  +|(name subproject_enforced_name)

Notice the key differences with --root:
- The workspace root changes from "workspace/" to "workspace/subproject/"
- Only configs within the new workspace are discovered
- The file path changes: "subproject/dune-project" vs "dune-project"

Verify that --root with an absolute path also works correctly:

  $ dunolint lint --root $PWD --dry-run
  dry-run: Would edit file "dune-project":
  -1,3 +1,3
    (lang dune 3.17)
    
  -|(name original_name)
  +|(name subproject_enforced_name)

Using --below from the parent directory. Config autoloading discovers both
parent and subproject configs, with subproject config taking precedence.

  $ cd ..
  $ dunolint lint --below $PWD/subproject --dry-run
  dry-run: Would edit file "subproject/dune-project":
  -1,3 +1,3
    (lang dune 3.17)
    
  -|(name original_name)
  +|(name subproject_enforced_name)

Now test that --config disables autoloading. When using --config, only that
specific config is used, and all auto-discovery is disabled.

  $ dunolint lint --config dunolint --dry-run
  dry-run: Would edit file "subproject/dune-project":
  -1,3 +1,3
    (lang dune 3.17)
    
  -|(name original_name)
  +|(name root_enforced_name)

Key takeaways:
- By default, configs are auto-discovered from workspace root down through subdirectories
- Deeper configs are applied last and take precedence
- --below limits which files are linted but doesn't affect config discovery
- --root changes the workspace root, limiting which configs can be discovered
- --config disables autoloading entirely and uses only the specified config
