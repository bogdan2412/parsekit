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

exception
  ParseError of
    { pos : int
    ; message : string
    }

type 'a t

val run : 'a t -> string -> require_input_entirely_consumed:bool -> 'a

(** Combinators *)

val fail : string -> _ t

include Monad.S with type 'a t := 'a t

val ( >> ) : 'a t -> 'b t -> 'b t
val ( << ) : 'a t -> 'b t -> 'a t
val many : 'a t -> at_least:int -> at_most:int option -> 'a list t
val sep_by : 'a t -> sep:_ t -> at_least:int -> at_most:int option -> 'a list t
val fix : ('a t -> 'a t) -> 'a t

(** Alternatives *)

val either : 'a t -> 'b t -> ('a, 'b) Either.t t
val choices : 'a t list -> 'a t

(** Prevents backtracking before the current point in the input. *)
val commit : unit t

(** Special parsers *)

val at_end_of_input : bool t
val end_of_input : unit t
val consumed_bytes : 'a t -> string t
val value_and_consumed_bytes : 'a t -> ('a * string) t

(** Basic parsers *)

val peek : len:int -> string option t
val take : len:int -> string t
val match_ : string -> string t
val take_while : f:(char -> bool) -> at_least:int -> at_most:int option -> string t

(** Matches any amount of whitespace characters, defined as one of [' '; '\t';
    '\n'; '\r'] *)
val whitespace0 : string t

(** Similar to [whitespace0], but requires at least one whitespace character to
    be present. *)
val whitespace : string t

val non_negative_integer : int t
val integer : int t
