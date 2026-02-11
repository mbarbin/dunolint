Dunolint simply skips over stanzas that it doesn't know about.

Initialize the project root.

  $ touch dune-workspace

Create some unknown/unsupported/invalid stanzas.

  $ cat > dune-project <<EOF
  > (lang dune 3.17)
  > 
  > (name main)
  > 
  > (() hello unknown stanza)
  > EOF

  $ mkdir -p lib/foo

  $ cat > lib/foo/dune <<EOF
  > (library
  >  (name bar))
  > 
  > ((unknown stanza) (are ignored))
  > EOF

This shouldn't however prevent dunolint from linting the other stanzas that it
knows about located in the same files.

  $ dunolint lint \
  >   --enforce='(dune (library (name (equals foo))))' \
  >   --enforce='(dune_project (name (equals foo)))' \
  >   --yes
  Editing file "dune-project":
  -1,5 +1,5
    (lang dune 3.17)
    
  -|(name main)
  +|(name foo)
    
    (() hello unknown stanza)
  
  Editing file "lib/foo/dune":
  -1,4 +1,4
    (library
  -| (name bar))
  +| (name foo))
    
    ((unknown stanza) (are ignored))
