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

[%%import "simd_support.h"]
[%%ifdef PARSEKIT_NO_SIMD]

let simd_supported = false

type t = unit

let simd8_width = 0
let raise_unsupported () = failwith "SIMD not supported"
let to_string _ = raise_unsupported ()
let create_exn ~init:_ = raise_unsupported ()
let create_repeat_array16_exn _ = raise_unsupported ()
let sexp_of_t _ = raise_unsupported ()
let unsafe_load_string ~dst:_ _ ~pos:_ = raise_unsupported ()
let land_ ~dst:_ _ _ = raise_unsupported ()
let lxor_ ~dst:_ _ _ = raise_unsupported ()
let lsr_ ~dst:_ _ _ = raise_unsupported ()
let saturating_sub ~dst:_ _ _ = raise_unsupported ()
let lookup16 ~dst:_ ~table:_ _ = raise_unsupported ()
let align_and_drop_right ~dst:_ _ _ _ = raise_unsupported ()
let no_bits_set _ = raise_unsupported ()

[%%else]

let simd_supported = true

type t

external simd8_width : unit -> int = "parsekit_simd8_width" [@@noalloc]

let simd8_width = simd8_width ()

external to_string : t -> string = "parsekit_simd_to_string"
external unsafe_create : init:int -> t = "parsekit_simd_unsafe_create"

let create_exn ~init =
  match 0 <= init && init <= 255 with
  | true -> unsafe_create ~init
  | false ->
    raise_s [%message "Value must be between [0] and [255] inclusive." (init : int)]
;;

external unsafe_create_repeat_array16
  :  int array
  -> t
  = "parsekit_simd_unsafe_create_repeat_array16"

let create_repeat_array16_exn array =
  match Array.length array with
  | 16 ->
    (match Array.for_all array ~f:(fun value -> 0 <= value && value <= 255) with
     | true -> unsafe_create_repeat_array16 array
     | false ->
       raise_s
         [%message
           "Array must contain only elements between [0] and [255] inclusive."
             (array : int array)])
  | _ -> raise_s [%message "Array must have length 16" ~len:(Array.length array : int)]
;;

let hex_digit value =
  match 0 <= value && value <= 9 with
  | true -> Char.unsafe_of_int (value + Char.to_int '0')
  | false -> Char.unsafe_of_int (value - 10 + Char.to_int 'a')
;;

let sexp_of_t t =
  let str = to_string t in
  let hex_bytes = Bytes.create (simd8_width * 2) in
  String.iteri str ~f:(fun idx chr ->
    let byte = Char.to_int chr in
    Bytes.unsafe_set hex_bytes (2 * idx) (hex_digit (byte lsr 4));
    Bytes.unsafe_set hex_bytes ((2 * idx) + 1) (hex_digit (byte land 0x0f)));
  Bytes.unsafe_to_string ~no_mutation_while_string_reachable:hex_bytes
  |> [%sexp_of: string]
;;

external unsafe_load_string
  :  dst:t
  -> string
  -> pos:int
  -> unit
  = "parsekit_simd_unsafe_load_string"
[@@noalloc]

external land_ : dst:t -> t -> t -> unit = "parsekit_simd_land" [@@noalloc]
external lxor_ : dst:t -> t -> t -> unit = "parsekit_simd_lxor" [@@noalloc]
external lsr_ : dst:t -> t -> int -> unit = "parsekit_simd_lsr" [@@noalloc]

external saturating_sub : dst:t -> t -> t -> unit = "parsekit_simd_saturating_sub"
[@@noalloc]

external lookup16 : dst:t -> table:t -> t -> unit = "parsekit_simd_lookup16" [@@noalloc]

external align_and_drop_right
  :  dst:t
  -> t
  -> t
  -> int
  -> unit
  = "parsekit_simd_align_and_drop_right"
[@@noalloc]

let align_and_drop_right ~dst t1 t2 drop =
  match 1 <= drop && drop <= 15 with
  | true -> align_and_drop_right ~dst t1 t2 drop
  | false ->
    raise_s
      [%message
        "[align_and_drop_right] must be called with a value of [drop] between [1] and \
         [15]."
          (drop : int)]
;;

external no_bits_set : t -> bool = "parsekit_simd_no_bits_set" [@@noalloc]

[%%endif]
