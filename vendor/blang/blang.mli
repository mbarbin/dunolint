(*_ The MIT License

  Copyright (c) 2008--2024 Jane Street Group, LLC
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

(** Boolean expressions. *)

(** A blang is a boolean expression built up by applying the usual boolean
    operations to properties that evaluate to true or false in some context.

    {2 Usage}

    For example, imagine writing a config file for an application that filters a
    stream of integers. Your goal is to keep only those integers that are
    multiples of either -3 or 5. Using [Blang] for this task, the code might
    look like:

    {[
      module Property = struct
        type t =
          | Multiple_of of int
          | Positive
          | Negative
        [@@deriving sexp]

        let eval t num =
          match t with
          | Multiple_of n -> num % n = 0
          | Positive      -> num > 0
          | Negative      -> num < 0
      end

      type config = {
        keep : Property.t Blang.t;
      } [@@deriving sexp]

      let config = {
        keep =
          Blang.t_of_sexp
            Property.t_of_sexp
            (Sexp.of_string
               "(or (and negative (multiple_of 3)) (and positive (multiple_of 5)))";
      }

      let keep config num : bool =
        Blang.eval config.keep (fun p -> Property.eval p num)
    ]}

    Note how [positive] and [negative] and [multiple_of] become operators in a
    small, newly-defined boolean expression language that allows you to write
    statements like [(and negative (multiple_of 3))].

    {2 Blang sexp syntax}

    The blang sexp syntax is almost exactly the derived one, except that:

    1. Base properties are not marked explicitly. Thus, if your base property
    type has elements FOO, BAR, etc., then you could write the following Blang
    s-expressions:

    {v
        FOO
        (and FOO BAR)
        (if FOO BAR BAZ)
    v}

    and so on. Note that this gets in the way of using the blang "keywords" in
    your value language.

    2. [And] and [Or] take a variable number of arguments, so that one can (and
    probably should) write

    {v (and FOO BAR BAZ QUX) v}

    instead of

    {v (and FOO (and BAR (and BAZ QUX))) v}

    If you want to see the derived sexp, use [Raw.sexp_of_t]. *)

(** Note that the sexps are not directly inferred from the type below -- there
    are lots of fancy shortcuts. Also, the sexps for ['a] must not look anything
    like blang sexps. Otherwise [t_of_sexp] will fail. The directly inferred
    sexps are available via [Raw.sexp_of_t]. *)
type +'a t = private
  | True
  | False
  | And of 'a t * 'a t
  | Or of 'a t * 'a t
  | Not of 'a t
  | If of 'a t * 'a t * 'a t
  | Base of 'a
[@@deriving compare, equal, sexp]

(** {2 Smart constructors that simplify away constants whenever possible} *)

module type Constructors = sig
  val base : 'a -> 'a t
  val true_ : _ t
  val false_ : _ t

  (** [function true -> true_ | false -> false_] *)
  val constant : bool -> _ t

  val not_ : 'a t -> 'a t

  (** n-ary [And] *)
  val and_ : 'a t list -> 'a t

  (** n-ary [Or] *)
  val or_ : 'a t list -> 'a t

  (** [if_ if then else] *)
  val if_ : 'a t -> 'a t -> 'a t -> 'a t
end

include Constructors

module O : sig
  include Constructors

  val ( && ) : 'a t -> 'a t -> 'a t
  val ( || ) : 'a t -> 'a t -> 'a t

  (** [a ==> b] is "a implies b". This is not [=>] to avoid making it look like
      a comparison operator. *)
  val ( ==> ) : 'a t -> 'a t -> 'a t

  val not : 'a t -> 'a t
end

(** [eval t f] evaluates the proposition [t] relative to an environment [f] that
    assigns truth values to base propositions. *)
val eval : 'a t -> ('a -> bool) -> bool
