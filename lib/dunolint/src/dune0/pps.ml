(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  Copyright (C) 2024-2025 Mathieu Barbin <mathieu.barbin@gmail.com>            *)
(*                                                                               *)
(*  This file is part of Dunolint.                                               *)
(*                                                                               *)
(*  Dunolint is free software; you can redistribute it and/or modify it          *)
(*  under the terms of the GNU Lesser General Public License as published by     *)
(*  the Free Software Foundation either version 3 of the License, or any later   *)
(*  version, with the LGPL-3.0 Linking Exception.                                *)
(*                                                                               *)
(*  Dunolint is distributed in the hope that it will be useful, but WITHOUT      *)
(*  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or        *)
(*  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License  *)
(*  and the file `NOTICE.md` at the root of this repository for more details.    *)
(*                                                                               *)
(*  You should have received a copy of the GNU Lesser General Public License     *)
(*  and the LGPL-3.0 Linking Exception along with this library. If not, see      *)
(*  <http://www.gnu.org/licenses/> and <https://spdx.org>, respectively.         *)
(*********************************************************************************)

open! Import

module Predicate = struct
  [@@@coverage off]

  module Param = struct
    type t =
      [ `any
      | `none
      | `some
      | `equals of string
      ]

    let compare =
      (fun a__001_ ->
         fun b__002_ ->
         if Stdlib.( == ) a__001_ b__002_
         then 0
         else (
           match a__001_, b__002_ with
           | `any, `any -> 0
           | `none, `none -> 0
           | `some, `some -> 0
           | `equals _left__003_, `equals _right__004_ ->
             compare_string _left__003_ _right__004_
           | x, y -> Stdlib.compare x y)
       : t -> t -> int)
    ;;

    let equal =
      (fun a__005_ ->
         fun b__006_ ->
         if Stdlib.( == ) a__005_ b__006_
         then true
         else (
           match a__005_, b__006_ with
           | `any, `any -> true
           | `none, `none -> true
           | `some, `some -> true
           | `equals _left__007_, `equals _right__008_ ->
             equal_string _left__007_ _right__008_
           | x, y -> Stdlib.( = ) x y)
       : t -> t -> bool)
    ;;

    let __t_of_sexp__ =
      (let error_source__014_ = "lib/dunolint/src/dune0/pps.ml.Predicate.Param.t" in
       function
       | Sexplib0.Sexp.Atom atom__010_ as _sexp__012_ ->
         (match atom__010_ with
          | "any" -> `any
          | "none" -> `none
          | "some" -> `some
          | "equals" ->
            Sexplib0.Sexp_conv_error.ptag_takes_args error_source__014_ _sexp__012_
          | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
       | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom atom__010_ :: sexp_args__013_) as
         _sexp__012_ ->
         (match atom__010_ with
          | "equals" as _tag__015_ ->
            (match sexp_args__013_ with
             | arg0__016_ :: [] ->
               let res0__017_ = string_of_sexp arg0__016_ in
               `equals res0__017_
             | _ ->
               Sexplib0.Sexp_conv_error.ptag_incorrect_n_args
                 error_source__014_
                 _tag__015_
                 _sexp__012_)
          | "any" -> Sexplib0.Sexp_conv_error.ptag_no_args error_source__014_ _sexp__012_
          | "none" -> Sexplib0.Sexp_conv_error.ptag_no_args error_source__014_ _sexp__012_
          | "some" -> Sexplib0.Sexp_conv_error.ptag_no_args error_source__014_ _sexp__012_
          | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
       | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__011_ ->
         Sexplib0.Sexp_conv_error.nested_list_invalid_poly_var
           error_source__014_
           sexp__011_
       | Sexplib0.Sexp.List [] as sexp__011_ ->
         Sexplib0.Sexp_conv_error.empty_list_invalid_poly_var
           error_source__014_
           sexp__011_
       : Sexplib0.Sexp.t -> t)
    ;;

    let t_of_sexp =
      (let error_source__019_ = "lib/dunolint/src/dune0/pps.ml.Predicate.Param.t" in
       fun sexp__018_ ->
         try __t_of_sexp__ sexp__018_ with
         | Sexplib0.Sexp_conv_error.No_variant_match ->
           Sexplib0.Sexp_conv_error.no_matching_variant_found
             error_source__019_
             sexp__018_
       : Sexplib0.Sexp.t -> t)
    ;;

    let sexp_of_t =
      (function
       | `any -> Sexplib0.Sexp.Atom "any"
       | `none -> Sexplib0.Sexp.Atom "none"
       | `some -> Sexplib0.Sexp.Atom "some"
       | `equals v__020_ ->
         Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "equals"; sexp_of_string v__020_ ]
       : t -> Sexplib0.Sexp.t)
    ;;
  end

  module Flag = struct
    module Applies_to = struct
      type t =
        [ `any
        | `driver
        | `pp of Pp.Name.t
        ]

      let compare =
        (fun a__021_ ->
           fun b__022_ ->
           if Stdlib.( == ) a__021_ b__022_
           then 0
           else (
             match a__021_, b__022_ with
             | `any, `any -> 0
             | `driver, `driver -> 0
             | `pp _left__023_, `pp _right__024_ ->
               Pp.Name.compare _left__023_ _right__024_
             | x, y -> Stdlib.compare x y)
         : t -> t -> int)
      ;;

      let equal =
        (fun a__025_ ->
           fun b__026_ ->
           if Stdlib.( == ) a__025_ b__026_
           then true
           else (
             match a__025_, b__026_ with
             | `any, `any -> true
             | `driver, `driver -> true
             | `pp _left__027_, `pp _right__028_ -> Pp.Name.equal _left__027_ _right__028_
             | x, y -> Stdlib.( = ) x y)
         : t -> t -> bool)
      ;;

      let __t_of_sexp__ =
        (let error_source__034_ =
           "lib/dunolint/src/dune0/pps.ml.Predicate.Flag.Applies_to.t"
         in
         function
         | Sexplib0.Sexp.Atom atom__030_ as _sexp__032_ ->
           (match atom__030_ with
            | "any" -> `any
            | "driver" -> `driver
            | "pp" ->
              Sexplib0.Sexp_conv_error.ptag_takes_args error_source__034_ _sexp__032_
            | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
         | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom atom__030_ :: sexp_args__033_) as
           _sexp__032_ ->
           (match atom__030_ with
            | "pp" as _tag__035_ ->
              (match sexp_args__033_ with
               | arg0__036_ :: [] ->
                 let res0__037_ = Pp.Name.t_of_sexp arg0__036_ in
                 `pp res0__037_
               | _ ->
                 Sexplib0.Sexp_conv_error.ptag_incorrect_n_args
                   error_source__034_
                   _tag__035_
                   _sexp__032_)
            | "any" ->
              Sexplib0.Sexp_conv_error.ptag_no_args error_source__034_ _sexp__032_
            | "driver" ->
              Sexplib0.Sexp_conv_error.ptag_no_args error_source__034_ _sexp__032_
            | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
         | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__031_ ->
           Sexplib0.Sexp_conv_error.nested_list_invalid_poly_var
             error_source__034_
             sexp__031_
         | Sexplib0.Sexp.List [] as sexp__031_ ->
           Sexplib0.Sexp_conv_error.empty_list_invalid_poly_var
             error_source__034_
             sexp__031_
         : Sexplib0.Sexp.t -> t)
      ;;

      let t_of_sexp =
        (let error_source__039_ =
           "lib/dunolint/src/dune0/pps.ml.Predicate.Flag.Applies_to.t"
         in
         fun sexp__038_ ->
           try __t_of_sexp__ sexp__038_ with
           | Sexplib0.Sexp_conv_error.No_variant_match ->
             Sexplib0.Sexp_conv_error.no_matching_variant_found
               error_source__039_
               sexp__038_
         : Sexplib0.Sexp.t -> t)
      ;;

      let sexp_of_t =
        (function
         | `any -> Sexplib0.Sexp.Atom "any"
         | `driver -> Sexplib0.Sexp.Atom "driver"
         | `pp v__040_ ->
           Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "pp"; Pp.Name.sexp_of_t v__040_ ]
         : t -> Sexplib0.Sexp.t)
      ;;
    end

    type t =
      { name : string
      ; param : Param.t
      ; applies_to : Applies_to.t
      }

    let compare =
      (fun a__041_ ->
         fun b__042_ ->
         if Stdlib.( == ) a__041_ b__042_
         then 0
         else (
           match compare_string a__041_.name b__042_.name with
           | 0 ->
             (match Param.compare a__041_.param b__042_.param with
              | 0 -> Applies_to.compare a__041_.applies_to b__042_.applies_to
              | n -> n)
           | n -> n)
       : t -> t -> int)
    ;;

    let equal =
      (fun a__043_ ->
         fun b__044_ ->
         if Stdlib.( == ) a__043_ b__044_
         then true
         else
           Stdlib.( && )
             (equal_string a__043_.name b__044_.name)
             (Stdlib.( && )
                (Param.equal a__043_.param b__044_.param)
                (Applies_to.equal a__043_.applies_to b__044_.applies_to))
       : t -> t -> bool)
    ;;

    let t_of_sexp =
      (let error_source__046_ = "lib/dunolint/src/dune0/pps.ml.Predicate.Flag.t" in
       fun x__047_ ->
         Sexplib0.Sexp_conv_record.record_of_sexp
           ~caller:error_source__046_
           ~fields:
             (Field
                { name = "name"
                ; kind = Required
                ; conv = string_of_sexp
                ; rest =
                    Field
                      { name = "param"
                      ; kind = Required
                      ; conv = Param.t_of_sexp
                      ; rest =
                          Field
                            { name = "applies_to"
                            ; kind = Required
                            ; conv = Applies_to.t_of_sexp
                            ; rest = Empty
                            }
                      }
                })
           ~index_of_field:(function
             | "name" -> 0
             | "param" -> 1
             | "applies_to" -> 2
             | _ -> -1)
           ~allow_extra_fields:false
           ~create:(fun (name, (param, (applies_to, ()))) ->
             ({ name; param; applies_to } : t))
           x__047_
       : Sexplib0.Sexp.t -> t)
    ;;

    let sexp_of_t =
      (fun { name = name__049_; param = param__051_; applies_to = applies_to__053_ } ->
         let bnds__048_ = ([] : _ Stdlib.List.t) in
         let bnds__048_ =
           let arg__054_ = Applies_to.sexp_of_t applies_to__053_ in
           (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "applies_to"; arg__054_ ]
            :: bnds__048_
            : _ Stdlib.List.t)
         in
         let bnds__048_ =
           let arg__052_ = Param.sexp_of_t param__051_ in
           (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "param"; arg__052_ ] :: bnds__048_
            : _ Stdlib.List.t)
         in
         let bnds__048_ =
           let arg__050_ = sexp_of_string name__049_ in
           (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "name"; arg__050_ ] :: bnds__048_
            : _ Stdlib.List.t)
         in
         Sexplib0.Sexp.List bnds__048_
       : t -> Sexplib0.Sexp.t)
    ;;
  end

  module Pp_with_flag = struct
    type t =
      { pp : Pp.Name.t
      ; flag : string
      ; param : Param.t
      }

    let compare =
      (fun a__055_ ->
         fun b__056_ ->
         if Stdlib.( == ) a__055_ b__056_
         then 0
         else (
           match Pp.Name.compare a__055_.pp b__056_.pp with
           | 0 ->
             (match compare_string a__055_.flag b__056_.flag with
              | 0 -> Param.compare a__055_.param b__056_.param
              | n -> n)
           | n -> n)
       : t -> t -> int)
    ;;

    let equal =
      (fun a__057_ ->
         fun b__058_ ->
         if Stdlib.( == ) a__057_ b__058_
         then true
         else
           Stdlib.( && )
             (Pp.Name.equal a__057_.pp b__058_.pp)
             (Stdlib.( && )
                (equal_string a__057_.flag b__058_.flag)
                (Param.equal a__057_.param b__058_.param))
       : t -> t -> bool)
    ;;

    let t_of_sexp =
      (let error_source__060_ =
         "lib/dunolint/src/dune0/pps.ml.Predicate.Pp_with_flag.t"
       in
       fun x__061_ ->
         Sexplib0.Sexp_conv_record.record_of_sexp
           ~caller:error_source__060_
           ~fields:
             (Field
                { name = "pp"
                ; kind = Required
                ; conv = Pp.Name.t_of_sexp
                ; rest =
                    Field
                      { name = "flag"
                      ; kind = Required
                      ; conv = string_of_sexp
                      ; rest =
                          Field
                            { name = "param"
                            ; kind = Required
                            ; conv = Param.t_of_sexp
                            ; rest = Empty
                            }
                      }
                })
           ~index_of_field:(function
             | "pp" -> 0
             | "flag" -> 1
             | "param" -> 2
             | _ -> -1)
           ~allow_extra_fields:false
           ~create:(fun (pp, (flag, (param, ()))) -> ({ pp; flag; param } : t))
           x__061_
       : Sexplib0.Sexp.t -> t)
    ;;

    let sexp_of_t =
      (fun { pp = pp__063_; flag = flag__065_; param = param__067_ } ->
         let bnds__062_ = ([] : _ Stdlib.List.t) in
         let bnds__062_ =
           let arg__068_ = Param.sexp_of_t param__067_ in
           (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "param"; arg__068_ ] :: bnds__062_
            : _ Stdlib.List.t)
         in
         let bnds__062_ =
           let arg__066_ = sexp_of_string flag__065_ in
           (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "flag"; arg__066_ ] :: bnds__062_
            : _ Stdlib.List.t)
         in
         let bnds__062_ =
           let arg__064_ = Pp.Name.sexp_of_t pp__063_ in
           (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "pp"; arg__064_ ] :: bnds__062_
            : _ Stdlib.List.t)
         in
         Sexplib0.Sexp.List bnds__062_
       : t -> Sexplib0.Sexp.t)
    ;;
  end

  type t =
    [ `pp of Pp.Name.t
    | `flag of Flag.t
    | `pp_with_flag of Pp_with_flag.t
    ]

  let compare =
    (fun a__069_ ->
       fun b__070_ ->
       if Stdlib.( == ) a__069_ b__070_
       then 0
       else (
         match a__069_, b__070_ with
         | `pp _left__071_, `pp _right__072_ -> Pp.Name.compare _left__071_ _right__072_
         | `flag _left__073_, `flag _right__074_ -> Flag.compare _left__073_ _right__074_
         | `pp_with_flag _left__075_, `pp_with_flag _right__076_ ->
           Pp_with_flag.compare _left__075_ _right__076_
         | x, y -> Stdlib.compare x y)
     : t -> t -> int)
  ;;

  let equal =
    (fun a__077_ ->
       fun b__078_ ->
       if Stdlib.( == ) a__077_ b__078_
       then true
       else (
         match a__077_, b__078_ with
         | `pp _left__079_, `pp _right__080_ -> Pp.Name.equal _left__079_ _right__080_
         | `flag _left__081_, `flag _right__082_ -> Flag.equal _left__081_ _right__082_
         | `pp_with_flag _left__083_, `pp_with_flag _right__084_ ->
           Pp_with_flag.equal _left__083_ _right__084_
         | x, y -> Stdlib.( = ) x y)
     : t -> t -> bool)
  ;;

  let __t_of_sexp__ =
    (let error_source__093_ = "lib/dunolint/src/dune0/pps.ml.Predicate.t" in
     function
     | Sexplib0.Sexp.Atom atom__086_ as _sexp__088_ ->
       (match atom__086_ with
        | "pp" -> Sexplib0.Sexp_conv_error.ptag_takes_args error_source__093_ _sexp__088_
        | "flag" ->
          Sexplib0.Sexp_conv_error.ptag_takes_args error_source__093_ _sexp__088_
        | "pp_with_flag" ->
          Sexplib0.Sexp_conv_error.ptag_takes_args error_source__093_ _sexp__088_
        | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
     | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom atom__086_ :: sexp_args__089_) as
       _sexp__088_ ->
       (match atom__086_ with
        | "pp" as _tag__097_ ->
          (match sexp_args__089_ with
           | arg0__098_ :: [] ->
             let res0__099_ = Pp.Name.t_of_sexp arg0__098_ in
             `pp res0__099_
           | _ ->
             Sexplib0.Sexp_conv_error.ptag_incorrect_n_args
               error_source__093_
               _tag__097_
               _sexp__088_)
        | "flag" as _tag__094_ ->
          (match sexp_args__089_ with
           | arg0__095_ :: [] ->
             let res0__096_ = Flag.t_of_sexp arg0__095_ in
             `flag res0__096_
           | _ ->
             Sexplib0.Sexp_conv_error.ptag_incorrect_n_args
               error_source__093_
               _tag__094_
               _sexp__088_)
        | "pp_with_flag" as _tag__090_ ->
          (match sexp_args__089_ with
           | arg0__091_ :: [] ->
             let res0__092_ = Pp_with_flag.t_of_sexp arg0__091_ in
             `pp_with_flag res0__092_
           | _ ->
             Sexplib0.Sexp_conv_error.ptag_incorrect_n_args
               error_source__093_
               _tag__090_
               _sexp__088_)
        | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
     | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__087_ ->
       Sexplib0.Sexp_conv_error.nested_list_invalid_poly_var error_source__093_ sexp__087_
     | Sexplib0.Sexp.List [] as sexp__087_ ->
       Sexplib0.Sexp_conv_error.empty_list_invalid_poly_var error_source__093_ sexp__087_
     : Sexplib0.Sexp.t -> t)
  ;;

  let t_of_sexp =
    (let error_source__101_ = "lib/dunolint/src/dune0/pps.ml.Predicate.t" in
     fun sexp__100_ ->
       try __t_of_sexp__ sexp__100_ with
       | Sexplib0.Sexp_conv_error.No_variant_match ->
         Sexplib0.Sexp_conv_error.no_matching_variant_found error_source__101_ sexp__100_
     : Sexplib0.Sexp.t -> t)
  ;;

  let sexp_of_t =
    (function
     | `pp v__102_ ->
       Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "pp"; Pp.Name.sexp_of_t v__102_ ]
     | `flag v__103_ ->
       Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "flag"; Flag.sexp_of_t v__103_ ]
     | `pp_with_flag v__104_ ->
       Sexplib0.Sexp.List
         [ Sexplib0.Sexp.Atom "pp_with_flag"; Pp_with_flag.sexp_of_t v__104_ ]
     : t -> Sexplib0.Sexp.t)
  ;;
end
