(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
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
     let config = Dunolinter.Config_handler.load_config_exn ~filename in
     if print
     then (
       let sexps = Dunolint.Config.to_stanzas config in
       print_endline
         (List.mapi sexps ~f:(fun i s ->
            (if i > 0 then "\n" else "") ^ Sexp.to_string_hum s)
          |> String.concat ~sep:"\n")))
;;
