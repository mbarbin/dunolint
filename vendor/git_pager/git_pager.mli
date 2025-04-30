(*_********************************************************************************)
(*_  Git_pager - Run a Git pager to display diffs and other custom outputs        *)
(*_  SPDX-FileCopyrightText: 2024-2025 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: MIT                                                 *)
(*_********************************************************************************)

(** Run a Git pager to display diffs and other custom outputs.

    The pager used is the one configured by Git. If [GIT_PAGER=cat], the pager
    is effectively disabled, and output is sent directly to stdout.

    This module is particularly useful for tools that integrate with Git and
    need to display output exceeding one screen, while respecting user color
    preferences and Git's configuration. *)

type t

(** Runs a Git pager and waits for its termination.

    Example:
    {[
      Git_pager.run ~f:(fun pager ->
        let write_end = Git_pager.write_end pager in
        Printf.fprintf write_end "Hello, pager!\n")
    ]} *)
val run : f:(t -> 'a) -> 'a

(** This is where lines to be sent to the pager should be written. You may flush
    the write end of the pager as needed, however note that [run] already ends
    with a call to [Out_channel.flush]. *)
val write_end : t -> Out_channel.t

(** {1 Color Handling} *)

(** Returns the appropriate color mode to use for Git commands whose outputs are
    sent to the pager.

    The color mode is decided based on:
    1. The command-line flag [--color=(auto|always|never)] (via [Err.color_mode]).
    2. Git's configuration for [color.ui].
    3. The output context (e.g., terminal, pager, or redirection).

    If both the command-line flag and [color.ui] are set to [auto], and the
    output is being sent to a pager, this function returns [`Always] to ensure
    colored output.

    Example:
    {[
      let git_diff_flags git_pager =
        match Git_pager.git_color_mode git_pager with
        | `Auto -> []
        | `Always -> [ "--color=always" ]
        | `Never -> [ "--color=never" ]
      ;;
    ]} *)
val git_color_mode : t -> [ `Auto | `Always | `Never ]

(** Based on the current color mode and output kind, says whether outputs sent
    to the pager should contain colors. It is useful for configuring custom
    outputs rendering or other non-Git commands. *)
val should_enable_color : t -> bool

(** Returns the kind of [t]'s output.

    - [`Tty]: Output is sent directly to a terminal (pager is disabled).
    - [`Pager]: Output is sent to a pager, as configured by Git.
    - [`Other]: Output is redirected (e.g., piped to another command or written to a file).

    See also {!should_enable_color}. *)
val output_kind : t -> [ `Tty | `Pager | `Other ]

(** {1 Private}

    This module is exported to be used by libraries with strong ties to
    [git-pager] and by tests. Its signature may change in breaking ways at any
    time without prior notice, and outside of the guidelines set by semver.

    Do not use. *)

module Private : sig
  (** So that we can exercise the pager logic in a test where stdout is never
      a tty, we export this reference to force the module behavior as if stdout
      was a tty. *)
  val force_stdout_isatty_test : bool ref
end
