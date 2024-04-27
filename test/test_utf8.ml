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

let%expect_test "of_code_exn edge conditions" =
  let pr t = Stdio.print_s [%sexp (t : Utf8_encoded.t)] in
  show_raise (fun () -> Utf8_encoded.of_code_exn (-1));
  [%expect {| (raised ("Invalid Unicode code point" (code -1))) |}];
  pr (Utf8_encoded.of_code_exn 0);
  [%expect {| "\000" |}];
  pr (Utf8_encoded.of_code_exn 127);
  [%expect {| "\127" |}];
  pr (Utf8_encoded.of_code_exn 128);
  [%expect {| "\194\128" |}];
  pr (Utf8_encoded.of_code_exn 0x7ff);
  [%expect {| "\223\191" |}];
  pr (Utf8_encoded.of_code_exn 0x800);
  [%expect {| "\224\160\128" |}];
  pr (Utf8_encoded.of_code_exn 0xfff);
  [%expect {| "\224\191\191" |}];
  pr (Utf8_encoded.of_code_exn 0x1000);
  [%expect {| "\225\128\128" |}];
  pr (Utf8_encoded.of_code_exn 0xd7ff);
  [%expect {| "\237\159\191" |}];
  show_raise (fun () -> Utf8_encoded.of_code_exn 0xd800);
  [%expect {| (raised ("Invalid Unicode code point" (code 55296))) |}];
  show_raise (fun () -> Utf8_encoded.of_code_exn 0xdfff);
  [%expect {| (raised ("Invalid Unicode code point" (code 57343))) |}];
  pr (Utf8_encoded.of_code_exn 0xe000);
  [%expect {| "\238\128\128" |}];
  pr (Utf8_encoded.of_code_exn 0xffff);
  [%expect {| "\239\191\191" |}];
  pr (Utf8_encoded.of_code_exn 0x10000);
  [%expect {| "\240\144\128\128" |}];
  pr (Utf8_encoded.of_code_exn 0x17fff);
  [%expect {| "\240\151\191\191" |}];
  pr (Utf8_encoded.of_code_exn 0x18000);
  [%expect {| "\240\152\128\128" |}];
  pr (Utf8_encoded.of_code_exn 0xfffff);
  [%expect {| "\243\191\191\191" |}];
  pr (Utf8_encoded.of_code_exn 0x100000);
  [%expect {| "\244\128\128\128" |}];
  pr (Utf8_encoded.of_code_exn 0x10ffff);
  [%expect {| "\244\143\191\191" |}];
  show_raise (fun () -> Utf8_encoded.of_code_exn 0x110000);
  [%expect {| (raised ("Invalid Unicode code point" (code 1114112))) |}];
  pr Utf8_encoded.replacement_character;
  [%expect {| "\239\191\189" |}]
;;

let%expect_test "round-trip" =
  for code = 0 to 0x10ffff do
    if code < 0xd800 || code > 0xdfff
    then (
      let t = Utf8_encoded.of_code_exn code in
      let code' = Utf8_encoded.to_code t in
      [%test_result: int] ~expect:code code';
      let string = Utf8_encoded.to_string t in
      let t' = Parsekit.run take1_utf8 string ~require_input_entirely_consumed:true in
      [%test_result: Utf8_encoded.t] ~expect:t t')
  done;
  [%expect {||}]
;;

let%expect_test "string input edge conditions" =
  let parser =
    buffered_output (fun [@inline] ~emit ->
      skip_many
        (let%map utf8_encoded, consumed_bytes = value_and_consumed_bytes take1_utf8 in
         (* Assuming the most recently processed byte sequence was valid (i.e. we did
            not emit a replacement_character), check that the value returned round trips
            through [code] and gets reserialized into the same byte sequence. *)
         (match Utf8_encoded.( <> ) utf8_encoded Utf8_encoded.replacement_character with
          | true ->
            [%test_result: string]
              ~expect:consumed_bytes
              (Utf8_encoded.to_string
                 (Utf8_encoded.of_code_exn (Utf8_encoded.to_code utf8_encoded)))
          | false -> ());
         Utf8_encoded.emit_encoded_data utf8_encoded ~emit)
        ~at_least:1
        ~at_most:None)
  in
  let run_parser string =
    Parsekit.run ~require_input_entirely_consumed:true parser string
  in
  let test string =
    let parsed_string = run_parser string in
    (* Ensure parser does not swallow any ASCII characters, which would be a security
       vulnerability. *)
    let parsed_string' = run_parser (string ^ "a") in
    [%test_result: string] ~expect:(parsed_string ^ "a") parsed_string'
  in
  let strings_around_value string =
    String.to_list string
    |> List.map ~f:(fun chr ->
      let chr = Char.to_int chr in
      [ Char.unsafe_of_int ((chr - 1) % 255)
      ; Char.unsafe_of_int (chr % 255)
      ; Char.unsafe_of_int ((chr + 1) % 255)
      ])
    |> List.fold_right ~init:[ [] ] ~f:(fun hd tl ->
      let%map.List hd = hd
      and tl = tl in
      hd :: tl)
    |> List.map ~f:String.of_char_list
  in
  let test_strings_around_code code =
    let string = Utf8_encoded.to_string (Utf8_encoded.of_code_exn code) in
    List.iter (strings_around_value string) ~f:test
  in
  test_strings_around_code 0;
  test_strings_around_code 127;
  test_strings_around_code 128;
  test_strings_around_code 0x7ff;
  test_strings_around_code 0x800;
  test_strings_around_code 0xfff;
  test_strings_around_code 0x1000;
  test_strings_around_code 0xd7ff;
  test_strings_around_code 0xe000;
  test_strings_around_code 0xffff;
  test_strings_around_code 0x10000;
  test_strings_around_code 0x17fff;
  test_strings_around_code 0x18000;
  test_strings_around_code 0xfffff;
  test_strings_around_code 0x100000;
  test_strings_around_code 0x10ffff;
  let random = Random.State.make [| 1337 |] in
  for _ = 1 to 100_000 do
    let len = Random.State.int_incl random 1 4 in
    let string =
      String.init len ~f:(fun index ->
        let char =
          match index + 1 = len with
          | true -> Random.State.int_incl random 0 255
          | false -> Random.State.int_incl random 128 255
        in
        Char.unsafe_of_int char)
    in
    List.iter (strings_around_value string) ~f:test
  done;
  [%expect {||}]
;;
