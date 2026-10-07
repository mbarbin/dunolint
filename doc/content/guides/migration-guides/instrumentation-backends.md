+++
title = "Instrumentation backends"
weight = 3
+++

## The change

*dunolint* now supports stanzas with several *instrumentation* fields, one per backend, as dune allows. Previously, it didn't consider that case. The `(dune (instrumentation _))` selector now considers these fields as a collection, which comes with the following changes to the `backend` predicate.

**Flags are matched as a subset.**

- **Before:** `(backend NAME FLAGS...)` held when the field had exactly these flags, and enforcing it replaced the flags of the field.
- **After:** `(backend NAME FLAGS...)` holds when there is a field for NAME with at least these flags, in any order. Enforcing it keeps the existing flags of the field, and adds the missing ones.

**Negating a backend without flags removes the field.**

- **Before:** `(not (backend NAME))` held when the field had NAME with some flags, and enforcing it never suggested any change.
- **After:** `(not (backend NAME))` holds when there is no field for NAME, regardless of its flags. Enforcing it removes the field, like `(absent NAME)`.

**Fields are not renamed.**

- **Before:** Enforcing `(backend NAME)` on a stanza with a field for another backend replaced the backend of that field.
- **After:** Enforcing `(backend NAME)` adds a field for NAME, and leaves the fields of other backends untouched.

## Do I need to migrate?

**Case 1: You use `backend` to require a backend**

Rules such as the one below keep working, and no migration is needed:

```dune
(rule
 (enforce (dune (instrumentation (backend bisect_ppx)))))
```

The difference is that a *bisect_ppx* field with flags now satisfies the rule, and its flags are kept.

**Case 2: You relied on `backend` to remove flags**

Enforcing `(backend NAME)` doesn't remove the flags of an existing field anymore. If you need these flags removed, you have to do it manually.

**Case 3: You relied on `backend` to replace a backend with another one**

Enforcing `(backend NAME)` now adds a field for NAME next to the existing one. To replace a backend with another one, require the new backend with `present`, and remove the old one with `absent`:

```dune
(rule
 (enforce
  (dune
   (instrumentation
    (and
     (present bisect_ppx)
     (absent landmarks))))))
```

See [Migrating to another backend](@/reference/config/dune.md#migrating-to-another-backend) for details.

**Case 4: You use `(not (backend NAME))`**

This now forbids any field for NAME, regardless of its flags, and enforcing it removes such a field. We recommend writing `(absent NAME)` instead, which is equivalent.
