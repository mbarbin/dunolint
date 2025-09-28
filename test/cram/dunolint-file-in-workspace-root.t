Test that a file named "dunolint" at the workspace root is loaded automatically.

Initialize the project root.

  $ touch dune-workspace

Create a dune-project file that needs linting.

  $ cat > dune-project <<EOF
  > (lang dune 3.17)
  > 
  > (name my_project_name)
  > EOF

Create a config file named "dunolint" at the workspace root.

  $ cat > dunolint <<EOF
  > (lang dunolint 1.0)
  > 
  > (rule
  >  (enforce (dune_project (name (equals corrected_name)))))
  > EOF

Running dunolint lint should automatically load the config from the workspace
root.

  $ dunolint lint --dry-run
  dry-run: Would edit file "dune-project":
  -1,3 +1,3
    (lang dune 3.17)
    
  -|(name my_project_name)
  +|(name corrected_name)

Test that explicit --config overrides the automatic loading.

  $ cat > explicit-config <<EOF
  > (lang dunolint 1.0)
  > 
  > (rule
  >  (enforce (dune_project (name (equals override_name)))))
  > EOF

  $ dunolint lint --config explicit-config --dry-run
  dry-run: Would edit file "dune-project":
  -1,3 +1,3
    (lang dune 3.17)
    
  -|(name my_project_name)
  +|(name override_name)

Test that a dunolint file in the current directory (if different from workspace
root) is NOT loaded automatically.

  $ mkdir subdir-with-config
  $ cd subdir-with-config
  $ cat > dunolint <<EOF
  > (lang dunolint 1.0)
  > 
  > (rule
  >  (enforce (dune_project (name (equals should_not_be_used)))))
  > EOF
  $ cat > dune-project <<EOF
  > (lang dune 3.17)
  > 
  > (name subdir_project)
  > EOF
  $ dunolint lint --dry-run
  Entering directory '$TESTCASE_ROOT'
  dry-run: Would edit file "dune-project":
  -1,3 +1,3
    (lang dune 3.17)
    
  -|(name my_project_name)
  +|(name corrected_name)
  
  dry-run: Would edit file "subdir-with-config/dune-project":
  -1,3 +1,3
    (lang dune 3.17)
    
  -|(name subdir_project)
  +|(name corrected_name)
  $ cd ..

The automatic loading only works for files named exactly "dunolint" at the
workspace root, not other names like ".dunolint" or "dunolint.config".

  $ mv dunolint dunolint.config
  $ dunolint lint --dry-run
  $ mv dunolint.config dunolint
