(** Parser combinator library for OCaml.

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

type t [@@immediate] [@@deriving sexp_of]

val replacement_character : t

include Comparable.S with type t := t
include Hashable.Key with type t := t

val to_code : t -> int
val of_code_exn : int -> t
val unchecked_of_code : int -> t
val to_string : t -> string

val encoded_data
  :  t
  -> ascii:(char -> 'a)
  -> two_byte:(char -> char -> 'a)
  -> three_byte:(char -> char -> char -> 'a)
  -> four_byte:(char -> char -> char -> char -> 'a)
  -> 'a

val emit_encoded_data : t -> emit:(char -> unit) -> unit

val parse_exn
  :  first_byte:char
  -> next_byte_exists:(unit -> bool)
  -> unsafe_peek:(unit -> char)
  -> unsafe_advance_byte:(unit -> unit)
  -> parse_error:(unit -> t)
  -> t
