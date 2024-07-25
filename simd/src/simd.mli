(** SIMD operations in OCaml.

    Copyright (C) 2024  Bogdan-Cristian Tataroiu

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>. *)

open! Base
open! Import

val simd_supported : bool
val simd8_width : int

type t [@@deriving sexp_of]

val to_string : t -> string

(** Requires a value between [0] and [255]. *)
val create_exn : init:int -> t

(** Requires an array of length [16], where every element is between [0] and [255]. *)
val create_repeat_array16_exn : int array -> t

(** Assumes [pos .. pos + simd8_width - 1] is within the bounds of the provided string. *)
val unsafe_load_string : dst:t -> string -> pos:int -> unit

val land_ : dst:t -> t -> t -> unit
val lxor_ : dst:t -> t -> t -> unit
val lsr_ : dst:t -> t -> int -> unit
val saturating_sub : dst:t -> t -> t -> unit
val lookup16 : dst:t -> table:t -> t -> unit
val align_and_drop_right : dst:t -> t -> t -> int -> unit
val no_bits_set : t -> bool
