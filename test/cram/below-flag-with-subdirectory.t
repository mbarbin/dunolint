Test the --below flag when running from a subdirectory of the workspace root.

Initialize the workspace root.

  $ touch dune-workspace
  $ ROOT=$(pwd)

Create a simple linting rule in the config.

  $ cat > dunolint <<EOF
  > (lang dunolint 1.0)
  > 
  > (rule
  >  (enforce (dune_project (name (equals fixed_name)))))
  > EOF

Create a directory structure with dune-project files at different levels.

  $ cat > dune-project <<EOF
  > (lang dune 3.17)
  > 
  > (name root_project)
  > EOF

  $ mkdir -p subdir1
  $ cat > subdir1/dune-project <<EOF
  > (lang dune 3.17)
  > 
  > (name subdir1_project)
  > EOF

  $ mkdir -p subdir2
  $ cat > subdir2/dune-project <<EOF
  > (lang dune 3.17)
  > 
  > (name subdir2_project)
  > EOF

  $ mkdir -p subdir1/nested
  $ cat > subdir1/nested/dune-project <<EOF
  > (lang dune 3.17)
  > 
  > (name nested_project)
  > EOF

Running dunolint lint from root affects all files.

  $ dunolint lint --dry-run
  dry-run: Would edit file "dune-project":
  -1,3 +1,3
    (lang dune 3.17)
    
  -|(name root_project)
  +|(name fixed_name)
  
  dry-run: Would edit file "subdir1/dune-project":
  -1,3 +1,3
    (lang dune 3.17)
    
  -|(name subdir1_project)
  +|(name fixed_name)
  
  dry-run: Would edit file "subdir1/nested/dune-project":
  -1,3 +1,3
    (lang dune 3.17)
    
  -|(name nested_project)
  +|(name fixed_name)
  
  dry-run: Would edit file "subdir2/dune-project":
  -1,3 +1,3
    (lang dune 3.17)
    
  -|(name subdir2_project)
  +|(name fixed_name)

Using --below . from a subdirectory only lints files in that subdirectory.

  $ cd subdir1
  $ dunolint lint --below . --dry-run
  Entering directory '$TESTCASE_ROOT'
  dry-run: Would edit file "subdir1/dune-project":
  -1,3 +1,3
    (lang dune 3.17)
    
  -|(name subdir1_project)
  +|(name fixed_name)
  
  dry-run: Would edit file "subdir1/nested/dune-project":
  -1,3 +1,3
    (lang dune 3.17)
    
  -|(name nested_project)
  +|(name fixed_name)

The root project and subdir2 are not affected because they're outside the --below path.

  $ cd ..
  $ cat dune-project | grep name
  (name root_project)
  $ cat subdir2/dune-project | grep name
  (name subdir2_project)

The --below flag now accepts absolute paths as well.

  $ dunolint lint --below ${ROOT}/subdir2 --dry-run
  dry-run: Would edit file "subdir2/dune-project":
  -1,3 +1,3
    (lang dune 3.17)
    
  -|(name subdir2_project)
  +|(name fixed_name)

Test absolute path for --below from a different directory:

  $ cd subdir1
  $ dunolint lint --below ${ROOT}/subdir2 --dry-run 2>&1
  Entering directory '$TESTCASE_ROOT'
  dry-run: Would edit file "subdir2/dune-project":
  -1,3 +1,3
    (lang dune 3.17)
    
  -|(name subdir2_project)
  +|(name fixed_name)
  $ cd ..

Using --below with a relative path from the root.

  $ dunolint lint --below subdir1/nested --dry-run
  dry-run: Would edit file "subdir1/nested/dune-project":
  -1,3 +1,3
    (lang dune 3.17)
    
  -|(name nested_project)
  +|(name fixed_name)

Test that --below flag fails when path is not in workspace. Create a directory
outside the workspace (as a sibling):

  $ mkdir -p ../outside_workspace
  $ cd ../outside_workspace
  $ dunolint lint --root ${ROOT} --below subdir2 --dry-run 2> output
  [123]
  $ grep -q "not in dune workspace" output
  $ cd ${ROOT}
  $ rm -rf ../outside_workspace

Test that paths are resolved correctly when using --below from a different cwd.

  $ cd ${ROOT}/subdir1
  $ dunolint lint --below ../subdir2 --dry-run 2>&1
  Entering directory '$TESTCASE_ROOT'
  dry-run: Would edit file "subdir2/dune-project":
  -1,3 +1,3
    (lang dune 3.17)
    
  -|(name subdir2_project)
  +|(name fixed_name)
