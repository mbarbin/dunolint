type t = |

let unreachable_code = function
  | (_ : t) -> .
;;

let sexp_of_t = unreachable_code
