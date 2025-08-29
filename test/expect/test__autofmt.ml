let%expect_test "maybe_autoformat_file" =
  let test ~previous_contents ~new_contents =
    let fmt =
      Dunolint_cli.Private.Linter.maybe_autoformat_file ~previous_contents ~new_contents
    in
    print_endline fmt
  in
  (* When the previous contents is well formatted, the new contents gets
     formatted as well. *)
  test
    ~previous_contents:
      (String.lstrip
         {|
(lang dune 3.17)

(name dunolint)
|})
    ~new_contents:"(lang    dune 3.17) (name dunolint)";
  [%expect
    {|
    (lang dune 3.17)

    (name dunolint)
    |}];
  (* If the previous contents is not formatted, then we do not autoformat the
     new one. This is a heuristic. *)
  test
    ~previous_contents:"(lang  dune 3.17) (name     dunolint)"
    ~new_contents:"(lang    dune 3.17) (name dunolint)";
  [%expect {| (lang    dune 3.17) (name dunolint) |}];
  (* This includes cases where the previous contents fails to autofmt. *)
  test
    ~previous_contents:"(lang  dune 3.17 invalid-file"
    ~new_contents:"(lang    dune 3.17) (name dunolint)";
  [%expect {| (lang    dune 3.17) (name dunolint) |}];
  ()
;;
