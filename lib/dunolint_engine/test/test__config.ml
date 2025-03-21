let eval_args args =
  let command =
    Cmdlang.Command.make Dunolint_engine.Config.arg ~summary:"eval-stdlib-runner"
  in
  match Cmdlang_stdlib_runner.eval command ~argv:(Array.of_list ("dunolint" :: args)) with
  | Ok t -> print_s [%sexp (t : Dunolint_engine.Config.t)]
  | Error (`Help msg) -> print_endline msg [@coverage off]
  | Error (`Bad msg) ->
    (Stdlib.print_string msg;
     print_s [%sexp "Evaluation Failed", { exit_code = (2 : int) }])
    [@coverage off]
  | exception e -> print_s [%sexp "Evaluation Raised", (e : Exn.t)]
;;

let%expect_test "running modes" =
  (* By default, when not running in an interactive terminal, the mode is to
     apply all lints without asking for confirmation.

     This is indeed what happens during the expect-test, because it is run in a
     context where stdout is not a tty. *)
  eval_args [];
  [%expect {| ((running_mode Force_yes)) |}];
  (* The interactive mode can be forced. *)
  eval_args [ "--interactive" ];
  [%expect {| ((running_mode Interactive)) |}];
  (* There are other running modes available. *)
  eval_args [ "--check" ];
  [%expect {| ((running_mode Check)) |}];
  eval_args [ "--dry-run" ];
  [%expect {| ((running_mode Dry_run)) |}];
  (* But they are mutually exclusive. *)
  eval_args [ "--check"; "--dry-run" ];
  [%expect
    {|
    ("Evaluation Raised" (
      "Conflicting flags [dry-run], [check]. Please choose one." (Exit 124)))
    |}];
  ()
;;

let%expect_test "create" =
  (* The API lets you create config programmatically. *)
  let t = Dunolint_engine.Config.create ~running_mode:Dry_run in
  print_s [%sexp (t : Dunolint_engine.Config.t)];
  [%expect {| ((running_mode Dry_run)) |}];
  ()
;;
