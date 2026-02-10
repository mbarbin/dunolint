# is_prefix/is_suffix on absent fields

## The change

**Before:** Enforcing `is_prefix` or `is_suffix` on an absent field was silently skipped.

**After:** Enforcing `is_prefix` or `is_suffix` on an absent field now causes an enforcement failure.

## Do I need to migrate?

Consider a rule like this:

```dune
(rule
 (enforce (dune (library (public_name (is_prefix "mylib."))))))
```

**Case 1: You intended to enforce the field's presence**

If your intent was that all libraries must have a `public_name` with this prefix, then no migration is needed. The new behavior is a bug fix: previously, libraries without `public_name` would silently pass when they should have failed. Now they correctly trigger an enforcement failure.

**Case 2: You intended conditional enforcement**

If your intent was to only check the prefix when `public_name` exists (and allow libraries without it), you need to migrate. Wrap the constraint with `if_present`:

```dune
(rule
 (enforce (dune (library (if_present (public_name (is_prefix "mylib.")))))))
```

The `if_present` wrapper applies the constraint only when the field exists. When the field is absent, the condition is satisfied and no enforcement failure occurs.

**Alternative:** For more complex conditions, you can use the explicit `if_` construct with `has_field`:

```dune
(rule
 (enforce
  (dune
   (library
    (if_ (has_field public_name)
         (public_name (is_prefix "mylib."))
         true_)))))
```
