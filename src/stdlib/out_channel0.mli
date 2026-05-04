(*_**************************************************************************************)
(*_  Dunolint_stdlib - Extending OCaml's Stdlib for Dunolint                            *)
(*_  SPDX-FileCopyrightText: 2025-2026 Mathieu Barbin <mathieu.barbin@gmail.com>        *)
(*_  SPDX-License-Identifier: MIT OR LGPL-3.0-or-later WITH LGPL-3.0-linking-exception  *)
(*_**************************************************************************************)

(*_ Notice: parts of this file are vendored from [Stdio.Out_channel], which we
  documented in the NOTICE at the root of the repo. The original license is
  reproduced in [third-party-license/janestreet/stdio] and below. See the
  [.ml] for per-binding provenance. *)

(*_ The MIT License

  Copyright (c) 2016--2024 Jane Street Group, LLC
  <opensource-contacts@janestreet.com>

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE. *)

include module type of struct
  include Stdlib.Out_channel
end

(** Opens [file] for writing. Diverges from [Stdio.Out_channel.create] by
    forcing binary mode (the only mode this project uses) and dropping
    [?fail_if_exists]; [?append] selects between [Open_append] and
    [Open_trunc]. *)
val create : ?append:bool -> ?perm:int -> string -> t

(** Opens a file for writing, calls [f] on the resulting channel, and closes it
    before returning [f]'s result, surfacing [f]'s and/or close-time exceptions
    to the caller. *)
val with_file : ?append:bool -> ?perm:int -> string -> f:(t -> 'a) -> 'a

val newline : t -> unit

(** Outputs a single line, terminated by a newline character. *)
val output_line : t -> string -> unit

val write_all : string -> data:string -> unit
