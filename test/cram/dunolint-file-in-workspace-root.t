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

Test that dunolint configs in subdirectories ARE discovered and loaded during
autoloading. When linting files in a subdirectory, both the workspace root
config and the subdirectory config are applied, with the subdirectory config
taking precedence for files in that directory.

  $ mkdir subdir-with-config
  $ cd subdir-with-config
  $ cat > dunolint <<EOF
  > (lang dunolint 1.0)
  > 
  > (rule
  >  (enforce (dune_project (name (equals subdir_enforced_name)))))
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
  +|(name subdir_enforced_name)
  $ cd ..

The automatic loading only works for files named exactly "dunolint". Files with
other names like ".dunolint" or "dunolint.config" are not discovered. Here we
rename the root config to verify it's no longer loaded, while the subdirectory
config continues to be loaded.

  $ mv dunolint dunolint.config
  $ dunolint lint --dry-run
  dry-run: Would edit file "subdir-with-config/dune-project":
  -1,3 +1,3
    (lang dune 3.17)
    
  -|(name subdir_project)
  +|(name subdir_enforced_name)
  $ mv dunolint.config dunolint
