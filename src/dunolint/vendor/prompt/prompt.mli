(*_********************************************************************************)
(*_  Dunolint - A tool to lint and help manage files in dune projects             *)
(*_  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*_                                                                               *)
(*_  This file was vendored from Prompt, whose license header is included below:  *)
(*_                                                                               *)
(*_  Prompt - A library to prompt the user for simple answers in the terminal     *)
(*_  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*_********************************************************************************)

(*_ This library was inspired by [async_interactive.v0.17.0].

  https://github.com/janestreet/async_interactive

  ----------------------------------------------------------------------------

  The MIT License

  Copyright (c) 2014--2024 Jane Street Group, LLC <opensource-contacts@janestreet.com>

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

(** A library to prompt the user for simple answers in the terminal. *)

val ask_gen : prompt:string -> f:(string -> ('a, string) Result.t) -> 'a

module Choice : sig
  type +'a t

  (** Use an uppercase [char] to indicate a default choice. There must
      be at most only one default for a given prompt, or the prompting
      function will raise [Invalid_argument]. *)
  val create : char -> 'a -> help:string -> 'a t

  (** Mark this choice as the default one. *)
  val default : 'a t -> 'a t
end

val ask : prompt:string -> choices:'a Choice.t list -> 'a
val ask_yn : prompt:string -> default:bool option -> bool

module Arg : sig
  val yes : bool Command.Arg.t
end

(** You can use this to insert style in the prompt. *)
val styled : Fmt.style -> string -> string
