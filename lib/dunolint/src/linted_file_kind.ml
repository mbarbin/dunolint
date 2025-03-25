module T = struct
  type t =
    [ `dune
    | `dune_project
    ]
  [@@deriving compare, enumerate, hash, sexp]

  let to_string = function
    | `dune -> "dune"
    | `dune_project -> "dune-project"
  ;;

  let of_string = function
    | "dune" -> Ok `dune
    | "dune-project" -> Ok `dune_project
    | str -> Error (`Msg (Printf.sprintf "Invalid linted file kind: %S" str))
  ;;
end

include T
include Comparable.Make (T)

let seeded_hash : int -> t -> int = Stdlib.Hashtbl.seeded_hash
