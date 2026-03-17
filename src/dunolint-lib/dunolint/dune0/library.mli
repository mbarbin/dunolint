(*_********************************************************************************)
(*_  Dunolint - A tool to lint and help manage files in dune projects             *)
(*_  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*_********************************************************************************)

module Modes = Library__modes
module Name = Library__name
module Package = Library__package
module Public_name = Library__public_name

module Predicate : sig
  type t =
    [ `has_field of
        [ `inline_tests
        | `instrumentation
        | `lint
        | `modes
        | `name
        | `package
        | `preprocess
        | `public_name
        ]
    | `instrumentation of Instrumentation.Predicate.t Blang.t
    | `libraries of Libraries.Predicate.t Blang.t
    | `lint of Lint.Predicate.t Blang.t
    | `modes of Modes.Predicate.t Blang.t
    | `name of Name.Predicate.t Blang.t
    | `package of Package.Predicate.t Blang.t
    | `preprocess of Preprocess.Predicate.t Blang.t
    | `public_name of Public_name.Predicate.t Blang.t
    | `if_present of
        [ `package of Package.Predicate.t Blang.t
        | `public_name of Public_name.Predicate.t Blang.t
        ]
    ]

  val equal : t -> t -> bool

  include Sexpable.S with type t := t
end
