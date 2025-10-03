Test that dunolint correctly lints files at the workspace root when run from a
subdirectory (due to automatic chdir to workspace root).

Initialize the workspace root.

  $ touch dune-workspace
  $ ROOT=$(pwd)

Create a config file that enforces specific project names.

  $ cat > dunolint <<EOF
  > (lang dunolint 1.0)
  > 
  > (rule
  >  (enforce (dune_project (name (equals workspace_name)))))
  > EOF

Create a dune-project file at the root that needs linting.

  $ cat > dune-project <<EOF
  > (lang dune 3.17)
  > 
  > (name root_project)
  > EOF

Create a library stanza at the root that needs linting.

  $ cat > dune <<EOF
  > (library
  >  (name rootlib)
  >  (libraries z a c b))
  > EOF

Create subdirectories with their own dune files.

  $ mkdir -p src/lib1
  $ cat > src/lib1/dune <<EOF
  > (library
  >  (name lib1)
  >  (libraries d c b a))
  > EOF

  $ mkdir -p src/lib2
  $ cat > src/lib2/dune-project <<EOF
  > (lang dune 3.17)
  > 
  > (name lib2_project)
  > EOF

Run dunolint from the root - everything should be linted.

  $ dunolint lint --dry-run 2>&1
  dry-run: Would edit file "dune":
  -1,3 +1,3
    (library
     (name rootlib)
  -| (libraries z a c b))
  +| (libraries a b c z))
  
  dry-run: Would edit file "dune-project":
  -1,3 +1,3
    (lang dune 3.17)
    
  -|(name root_project)
  +|(name workspace_name)
  
  dry-run: Would edit file "src/lib1/dune":
  -1,3 +1,3
    (library
     (name lib1)
  -| (libraries d c b a))
  +| (libraries a b c d))
  
  dry-run: Would edit file "src/lib2/dune-project":
  -1,3 +1,3
    (lang dune 3.17)
    
  -|(name lib2_project)
  +|(name workspace_name)

Now run dunolint from a subdirectory - it should still lint all files including
the root files because it changes directory to the workspace root.

  $ cd src/lib1
  $ dunolint lint --dry-run 2>&1
  Entering directory '$TESTCASE_ROOT'
  dry-run: Would edit file "dune":
  -1,3 +1,3
    (library
     (name rootlib)
  -| (libraries z a c b))
  +| (libraries a b c z))
  
  dry-run: Would edit file "dune-project":
  -1,3 +1,3
    (lang dune 3.17)
    
  -|(name root_project)
  +|(name workspace_name)
  
  dry-run: Would edit file "src/lib1/dune":
  -1,3 +1,3
    (library
     (name lib1)
  -| (libraries d c b a))
  +| (libraries a b c d))
  
  dry-run: Would edit file "src/lib2/dune-project":
  -1,3 +1,3
    (lang dune 3.17)
    
  -|(name lib2_project)
  +|(name workspace_name)

Notice that the root dune-project and dune files are correctly linted even
though we're running from src/lib1. This confirms the chdir behavior.

Test from a deeply nested directory.

  $ mkdir -p $ROOT/src/lib1/nested/deep
  $ cd $ROOT/src/lib1/nested/deep
  $ cat > dune <<EOF
  > (library
  >  (name deep)
  >  (libraries x y z))
  > EOF

  $ dunolint lint --dry-run 2>&1
  Entering directory '$TESTCASE_ROOT'
  dry-run: Would edit file "dune":
  -1,3 +1,3
    (library
     (name rootlib)
  -| (libraries z a c b))
  +| (libraries a b c z))
  
  dry-run: Would edit file "dune-project":
  -1,3 +1,3
    (lang dune 3.17)
    
  -|(name root_project)
  +|(name workspace_name)
  
  dry-run: Would edit file "src/lib1/dune":
  -1,3 +1,3
    (library
     (name lib1)
  -| (libraries d c b a))
  +| (libraries a b c d))
  
  dry-run: Would edit file "src/lib2/dune-project":
  -1,3 +1,3
    (lang dune 3.17)
    
  -|(name lib2_project)
  +|(name workspace_name)

Again, the root files are correctly linted despite running from a deeply nested
directory.

Test that --below can limit the scope even when running from a subdirectory.

  $ cd $ROOT/src/lib1
  $ dunolint lint --below . --dry-run 2>&1
  Entering directory '$TESTCASE_ROOT'
  dry-run: Would edit file "src/lib1/dune":
  -1,3 +1,3
    (library
     (name lib1)
  -| (libraries d c b a))
  +| (libraries a b c d))

This shows that --below correctly limits the scope to src/lib1 and its
subdirectories, excluding the root files and src/lib2.
