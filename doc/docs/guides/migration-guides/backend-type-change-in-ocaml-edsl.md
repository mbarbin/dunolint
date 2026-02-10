# Backend type change in OCaml EDSL

## The change

The `backend` EDSL constructor now accepts a `Backend.t` (which includes name and flags) instead of a `Backend.Name.t`.

**Sexp configs are not affected.** If you write your dunolint config using s-expressions, this change is backward compatible -- `(backend bisect_ppx)` still works.

**OCaml EDSL users must update.** If you build predicates in OCaml, the constructor changed from `Backend.Name.v` to `Backend.v`.

## Migration

Replace `Backend.Name.v` with `Backend.v`:

```ocaml
(* Before *)
let bisect_ppx = Dune.Instrumentation.Backend.Name.v "bisect_ppx"

(* After *)
let bisect_ppx = Dune.Instrumentation.Backend.v "bisect_ppx"
```

To specify backend flags, use the optional `~flags` parameter:

```ocaml
let ppx_windtrap = Dune.Instrumentation.Backend.v "ppx_windtrap" ~flags:["--coverage"]
```
