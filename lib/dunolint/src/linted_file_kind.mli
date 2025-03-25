type t =
  [ `dune
  | `dune_project
  ]
[@@deriving enumerate]

val to_string : t -> string
val of_string : string -> (t, [ `Msg of string ]) Result.t

include Container_key.S with type t := t
include Comparable.S with type t := t
