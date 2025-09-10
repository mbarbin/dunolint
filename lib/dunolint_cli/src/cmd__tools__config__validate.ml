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

let main =
  Command.make
    ~summary:"Validate the supplied config file."
    ~readme:(fun () ->
      "You can use this command to validate that the supplied file is a valid config \
       file for $(b,dunolint).")
    (let open Command.Std in
     let+ filename = Arg.pos ~pos:0 Param.file ~doc:"Config file to customize dunolint."
     and+ print =
       Arg.flag [ "print" ] ~doc:"Print the parsed config as a S-expression."
     in
     let config = Common_helpers.load_config_exn ~filename in
     if print
     then (
       let sexps = Dunolint.Config.to_stanzas config in
       print_endline
         (List.mapi sexps ~f:(fun i s ->
            (if i > 0 then "\n" else "") ^ Sexp.to_string_hum s)
          |> String.concat ~sep:"\n")))
;;
