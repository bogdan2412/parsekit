(** Quickcheck methods for types defined by Parsekit

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
include Json_parsekit

module For_quickcheck = struct
  module Finite_float = struct
    type t = float

    let quickcheck_generator = Generator.float_finite
    let quickcheck_observer = Observer.float
    let quickcheck_shrinker = Shrinker.float
  end

  module Utf8_string = struct
    type t = string

    let quickcheck_generator =
      let%bind.Generator length = Generator.small_positive_or_zero_int in
      let%map.Generator list =
        Generator.list_with_length ~length Utf8_encoded.quickcheck_generator
      in
      let buffer = Buffer.create 128 in
      List.iter list ~f:(fun code ->
        Utf8_encoded.emit_encoded_data code ~emit:(Buffer.add_char buffer));
      Buffer.contents buffer
    ;;

    let quickcheck_observer = Observer.string

    let quickcheck_shrinker =
      Shrinker.fixed_point (fun string_shrinker ->
        Shrinker.create (fun string ->
          match String.is_empty string with
          | true -> Sequence.empty
          | false ->
            let head, tail =
              Utf8_encoded.parse_single
                string
                ~pos:0
                ~len:(String.length string)
                ~on_valid:(fun ~consumed (_ : Utf8_encoded.t) ->
                  ( String.sub string ~pos:0 ~len:consumed
                  , String.sub string ~pos:consumed ~len:(String.length string - consumed)
                  ))
                ~on_invalid:(fun ~consumed:_ -> failwith "Invalid UTF8 encoded string")
            in
            Sequence.round_robin
              [ Sequence.singleton tail
              ; Sequence.map (Shrinker.shrink string_shrinker tail) ~f:(fun tail ->
                  String.append head tail)
              ]))
    ;;
  end

  type t =
    | Null
    | Bool of bool
    | Number of Finite_float.t
    | String of Utf8_string.t
    | List of t list
    | Dictionary of (Utf8_string.t * t) list
  [@@deriving quickcheck]
end

let rec of_for_quickcheck (value : For_quickcheck.t) : t =
  match value with
  | Null -> Null
  | Bool value -> Bool value
  | Number value -> Number value
  | String value -> String value
  | List values -> List (List.map values ~f:of_for_quickcheck)
  | Dictionary values ->
    Dictionary
      (List.map values ~f:(fun (key, data) -> key, of_for_quickcheck data)
       |> Map.of_alist_reduce (module String) ~f:(fun _ snd -> snd))
;;

let rec to_for_quickcheck (value : t) : For_quickcheck.t =
  match value with
  | Null -> Null
  | Bool value -> Bool value
  | Number value -> Number value
  | String value -> String value
  | List values -> List (List.map values ~f:to_for_quickcheck)
  | Dictionary values ->
    Dictionary
      (Map.to_alist values |> List.map ~f:(fun (key, data) -> key, to_for_quickcheck data))
;;

let quickcheck_generator =
  let%map.Generator value = For_quickcheck.quickcheck_generator in
  of_for_quickcheck value
;;

let quickcheck_observer =
  Observer.unmap For_quickcheck.quickcheck_observer ~f:to_for_quickcheck
;;

let quickcheck_shrinker =
  Shrinker.map
    For_quickcheck.quickcheck_shrinker
    ~f:of_for_quickcheck
    ~f_inverse:to_for_quickcheck
;;
