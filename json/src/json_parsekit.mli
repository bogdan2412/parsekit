(** JSON parsing library built using the Parsekit library.

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

type t =
  | Null
  | Bool of bool
  | Number of float
  | String of string
  | List of t list
  | Dictionary of t Map.M(String).t
[@@deriving compare, hash, sexp_of]

val parser : t Parsekit.t
val serialize : encoding_format:[ `Ascii | `Utf8 ] -> t -> string
val null_exn : t -> unit
val bool_exn : t -> bool
val number_exn : t -> float
val string_exn : t -> string
val list_exn : t -> t list
val dictionary_exn : t -> t Map.M(String).t
val dictionary_member_exn : t -> key:string -> t

module Parser : sig
  val null : unit Parsekit.t
  val bool : bool Parsekit.t
  val number : float Parsekit.t
  val string : string Parsekit.t
  val list : t list Parsekit.t
  val dictionary : t Map.M(String).t Parsekit.t

  module Custom : sig
    val list' : data:t Parsekit.t -> t list Parsekit.t

    val dictionary'
      :  key:string Parsekit.t
      -> data:t Parsekit.t
      -> t Map.M(String).t Parsekit.t
  end
end

module Serializer : sig
  val null : Buffer.t -> unit
  val bool : Buffer.t -> bool -> unit
  val number : Buffer.t -> float -> unit
  val string : encoding_format:[ `Ascii | `Utf8 ] -> Buffer.t -> string -> unit
  val list : encoding_format:[ `Ascii | `Utf8 ] -> Buffer.t -> t list -> unit

  val dictionary
    :  encoding_format:[ `Ascii | `Utf8 ]
    -> Buffer.t
    -> t Map.M(String).t
    -> unit

  val json : encoding_format:[ `Ascii | `Utf8 ] -> Buffer.t -> t -> unit
end
