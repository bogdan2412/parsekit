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
include Parsekit.Utf8_encoded

let quickcheck_generator =
  let ranges_per_encoding_length =
    [ [ 0, 0x7f ]
    ; [ 0x80, 0x7ff ]
    ; [ 0x800, 0xd800 - 1; 0xdfff + 1, 0xffff ]
    ; [ 0x10000, 0x10ffff ]
    ]
  in
  assert (
    let expected_code_count = 0x10ffff + 1 - (0xdfff - 0xd800 + 1) in
    let covered_code_count =
      List.fold ranges_per_encoding_length ~init:0 ~f:(fun acc ranges ->
        List.fold ranges ~init:acc ~f:(fun acc (lower, upper) -> acc + upper - lower + 1))
    in
    Int.( = ) expected_code_count covered_code_count);
  let%bind.Generator ranges = Generator.of_list ranges_per_encoding_length in
  let%bind.Generator lower, upper = Generator.of_list ranges in
  let%map.Generator code = Generator.int_inclusive lower upper in
  Parsekit.Utf8_encoded.of_code_exn code
;;

let quickcheck_observer = Observer.of_hash_fold Parsekit.Utf8_encoded.hash_fold_t
let quickcheck_shrinker = Shrinker.atomic
