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

let%expect_test "Data type examples" =
  let input =
    {|
    {
      "null": null,
      "true": true,
      "false": false,
      "array": [ 1, 2, 3, 4, "5", "6", "7", "8" ],
      "object": {
        "number": 1234,
        "negative_number": -1234,
        "floating_point": 123.456,
        "big_number": 12345678901234567890,
        "float_with_exp": 1e-100,
        "float_with_exp_and_frac": 1.2345e10,
        "negative_float_with_exp_and_frac": -1.2345e10,
        "string": "example string"
      },
      "string escaping": "\"\\\/\b\f\n\r\t",
      "single_byte_utf8": "\u0061",
      "two_byte_utf8": "\u00BC \u0219",
      "three_byte_utf8": "\u2E26 \uFB04",
      "four_byte_utf8": "\uD83D\uDE80",
      "utf8_boundaries": "\u0000 \u007f \u0080 \u07ff \u0800 \uffff \ud800\udc00 \udbff\udfff"
    }
  |}
  in
  let t = Parsekit.run Json.parser input ~require_input_entirely_consumed:true in
  print_s [%sexp (t : Json.t)];
  [%expect
    {|
    (Dictionary
     ((array
       (List
        ((Number 1) (Number 2) (Number 3) (Number 4) (String 5) (String 6)
         (String 7) (String 8))))
      (false (Bool false)) (four_byte_utf8 (String "\240\159\154\128"))
      (null Null)
      (object
       (Dictionary
        ((big_number (Number 1.2345678901234567E+19))
         (float_with_exp (Number 1E-100))
         (float_with_exp_and_frac (Number 12345000000))
         (floating_point (Number 123.456))
         (negative_float_with_exp_and_frac (Number -12345000000))
         (negative_number (Number -1234)) (number (Number 1234))
         (string (String "example string")))))
      (single_byte_utf8 (String a))
      ("string escaping" (String  "\"\\/\b\012\
                                 \n\r\t"))
      (three_byte_utf8 (String "\226\184\166 \239\172\132")) (true (Bool true))
      (two_byte_utf8 (String "\194\188 \200\153"))
      (utf8_boundaries
       (String
        "\000 \127 \194\128 \223\191 \224\160\128 \239\191\191 \240\144\128\128 \244\143\191\191")))) |}];
  print_endline (Json.dictionary_member_exn t ~key:"single_byte_utf8" |> Json.string_exn);
  print_endline (Json.dictionary_member_exn t ~key:"two_byte_utf8" |> Json.string_exn);
  print_endline (Json.dictionary_member_exn t ~key:"three_byte_utf8" |> Json.string_exn);
  print_endline (Json.dictionary_member_exn t ~key:"four_byte_utf8" |> Json.string_exn);
  [%expect {|
    a
    ¼ ș
    ⸦ ﬄ
    🚀 |}]
;;

let%expect_test "last value wins on duplicate keys" =
  let input = {| { "a": 1, "a": 2 } |} in
  let t = Parsekit.run Json.parser input ~require_input_entirely_consumed:true in
  print_s [%sexp (t : Json.t)];
  [%expect {| (Dictionary ((a (Number 2)))) |}];
  let input = {| { "\t": 1, "\u0009": 2 } |} in
  let t = Parsekit.run Json.parser input ~require_input_entirely_consumed:true in
  print_s [%sexp (t : Json.t)];
  [%expect {| (Dictionary (("\t" (Number 2)))) |}]
;;

let%expect_test "compliance tests" =
  let dir = "compliance" in
  let test_suite =
    Array.to_list (Stdlib.Sys.readdir dir)
    |> List.filter ~f:(fun name ->
      String.( <> ) name "LICENSE" && String.( <> ) name "ACKNOWLEDGEMENTS")
    |> List.sort ~compare:String.compare
  in
  let should_pass, should_fail, implementation_defined =
    List.partition3_map test_suite ~f:(fun name ->
      match String.get name 0 with
      | 'y' -> `Fst name
      | 'n' -> `Snd name
      | 'i' -> `Trd name
      | _ -> raise_s [%message "Unexpected test name" (name : string)])
  in
  let report name output =
    let name = String.chop_suffix_exn name ~suffix:".json" ^ ":" in
    print_endline [%string "%{name#:58} %{output}"]
  in
  print_endline "# Valid JSON tests";
  List.iter should_pass ~f:(fun name ->
    let path = Stdlib.Filename.concat dir name in
    let input = Stdio.In_channel.read_all path in
    match Parsekit.run Json.parser input ~require_input_entirely_consumed:true with
    | exception _ -> report name "FAIL"
    | _ -> report name "PASS");
  print_endline "";
  print_endline "# Invalid JSON tests";
  List.iter should_fail ~f:(fun name ->
    let path = Stdlib.Filename.concat dir name in
    let input = Stdio.In_channel.read_all path in
    match Parsekit.run Json.parser input ~require_input_entirely_consumed:true with
    | exception _ -> report name "PASS"
    | _ -> report name "FAIL");
  print_endline "";
  print_endline "# Implementation defined JSON tests";
  List.iter implementation_defined ~f:(fun name ->
    let path = Stdlib.Filename.concat dir name in
    let input = Stdio.In_channel.read_all path in
    match Parsekit.run Json.parser input ~require_input_entirely_consumed:true with
    | exception _ -> report name "DOES NOT PARSE"
    | _ -> report name "PARSES");
  [%expect
    {|
    # Valid JSON tests
                                     y_array_arraysWithSpaces: PASS
                                         y_array_empty-string: PASS
                                                y_array_empty: PASS
                                  y_array_ending_with_newline: PASS
                                                y_array_false: PASS
                                        y_array_heterogeneous: PASS
                                                 y_array_null: PASS
                                   y_array_with_1_and_newline: PASS
                                   y_array_with_leading_space: PASS
                                    y_array_with_several_null: PASS
                                  y_array_with_trailing_space: PASS
                                                     y_number: PASS
                                                y_number_0e+1: PASS
                                                 y_number_0e1: PASS
                                         y_number_after_space: PASS
                                y_number_double_close_to_zero: PASS
                                        y_number_int_with_exp: PASS
                                          y_number_minus_zero: PASS
                                        y_number_negative_int: PASS
                                        y_number_negative_one: PASS
                                       y_number_negative_zero: PASS
                                      y_number_real_capital_e: PASS
                              y_number_real_capital_e_neg_exp: PASS
                              y_number_real_capital_e_pos_exp: PASS
                                       y_number_real_exponent: PASS
                              y_number_real_fraction_exponent: PASS
                                        y_number_real_neg_exp: PASS
                                   y_number_real_pos_exponent: PASS
                                          y_number_simple_int: PASS
                                         y_number_simple_real: PASS
                                                     y_object: PASS
                                               y_object_basic: PASS
                                      y_object_duplicated_key: PASS
                            y_object_duplicated_key_and_value: PASS
                                               y_object_empty: PASS
                                           y_object_empty_key: PASS
                                 y_object_escaped_null_in_key: PASS
                                     y_object_extreme_numbers: PASS
                                        y_object_long_strings: PASS
                                              y_object_simple: PASS
                                      y_object_string_unicode: PASS
                                       y_object_with_newlines: PASS
                         y_string_1_2_3_bytes_UTF-8_sequences: PASS
                             y_string_accepted_surrogate_pair: PASS
                            y_string_accepted_surrogate_pairs: PASS
                                     y_string_allowed_escapes: PASS
                        y_string_backslash_and_u_escaped_zero: PASS
                              y_string_backslash_doublequotes: PASS
                                            y_string_comments: PASS
                                     y_string_double_escape_a: PASS
                                     y_string_double_escape_n: PASS
                           y_string_escaped_control_character: PASS
                                y_string_escaped_noncharacter: PASS
                                            y_string_in_array: PASS
                         y_string_in_array_with_leading_space: PASS
                             y_string_last_surrogates_1_and_2: PASS
                                       y_string_nbsp_uescaped: PASS
                        y_string_nonCharacterInUTF-8_U+10FFFF: PASS
                          y_string_nonCharacterInUTF-8_U+FFFF: PASS
                                         y_string_null_escape: PASS
                                      y_string_one-byte-utf-8: PASS
                                                  y_string_pi: PASS
                    y_string_reservedCharacterInUTF-8_U+1BFFF: PASS
                                        y_string_simple_ascii: PASS
                                               y_string_space: PASS
            y_string_surrogates_U+1D11E_MUSICAL_SYMBOL_G_CLEF: PASS
                                    y_string_three-byte-utf-8: PASS
                                      y_string_two-byte-utf-8: PASS
                                     y_string_u+2028_line_sep: PASS
                                      y_string_u+2029_par_sep: PASS
                                             y_string_uEscape: PASS
                                    y_string_uescaped_newline: PASS
                               y_string_unescaped_char_delete: PASS
                                             y_string_unicode: PASS
                             y_string_unicodeEscapedBackslash: PASS
                                           y_string_unicode_2: PASS
                            y_string_unicode_U+10FFFE_nonchar: PASS
                             y_string_unicode_U+1FFFE_nonchar: PASS
                     y_string_unicode_U+200B_ZERO_WIDTH_SPACE: PASS
                       y_string_unicode_U+2064_invisible_plus: PASS
                              y_string_unicode_U+FDD0_nonchar: PASS
                              y_string_unicode_U+FFFE_nonchar: PASS
                        y_string_unicode_escaped_double_quote: PASS
                                                y_string_utf8: PASS
                                  y_string_with_del_character: PASS
                                     y_structure_lonely_false: PASS
                                       y_structure_lonely_int: PASS
                             y_structure_lonely_negative_real: PASS
                                      y_structure_lonely_null: PASS
                                    y_structure_lonely_string: PASS
                                      y_structure_lonely_true: PASS
                                     y_structure_string_empty: PASS
                                 y_structure_trailing_newline: PASS
                                    y_structure_true_in_array: PASS
                                 y_structure_whitespace_array: PASS

    # Invalid JSON tests
                                 n_array_1_true_without_comma: PASS
                                       n_array_a_invalid_utf8: PASS
                               n_array_colon_instead_of_comma: PASS
                                    n_array_comma_after_close: PASS
                                     n_array_comma_and_number: PASS
                                         n_array_double_comma: PASS
                                   n_array_double_extra_comma: PASS
                                          n_array_extra_close: PASS
                                          n_array_extra_comma: PASS
                                           n_array_incomplete: PASS
                             n_array_incomplete_invalid_value: PASS
                                 n_array_inner_array_no_comma: PASS
                                         n_array_invalid_utf8: PASS
                         n_array_items_separated_by_semicolon: PASS
                                           n_array_just_comma: PASS
                                           n_array_just_minus: PASS
                                        n_array_missing_value: PASS
                                    n_array_newlines_unclosed: PASS
                                     n_array_number_and_comma: PASS
                            n_array_number_and_several_commas: PASS
                         n_array_spaces_vertical_tab_formfeed: PASS
                                          n_array_star_inside: PASS
                                             n_array_unclosed: PASS
                              n_array_unclosed_trailing_comma: PASS
                              n_array_unclosed_with_new_lines: PASS
                          n_array_unclosed_with_object_inside: PASS
                                           n_incomplete_false: PASS
                                            n_incomplete_null: PASS
                                            n_incomplete_true: PASS
                                  n_multidigit_number_then_00: PASS
                                                  n_number_++: PASS
                                                  n_number_+1: PASS
                                                n_number_+Inf: PASS
                                                 n_number_-01: PASS
                                               n_number_-1.0.: PASS
                                                 n_number_-2.: PASS
                                                n_number_-NaN: PASS
                                                 n_number_.-1: PASS
                                               n_number_.2e-3: PASS
                                               n_number_0.1.2: PASS
                                               n_number_0.3e+: PASS
                                                n_number_0.3e: PASS
                                                n_number_0.e1: PASS
                                        n_number_0_capital_E+: PASS
                                         n_number_0_capital_E: PASS
                                                 n_number_0e+: PASS
                                                  n_number_0e: PASS
                                               n_number_1.0e+: PASS
                                               n_number_1.0e-: PASS
                                                n_number_1.0e: PASS
                                               n_number_1_000: PASS
                                                n_number_1eE2: PASS
                                               n_number_2.e+3: PASS
                                               n_number_2.e-3: PASS
                                                n_number_2.e3: PASS
                                                n_number_9.e+: PASS
                                                 n_number_Inf: PASS
                                                 n_number_NaN: PASS
                          n_number_U+FF11_fullwidth_digit_one: PASS
                                          n_number_expression: PASS
                                         n_number_hex_1_digit: PASS
                                        n_number_hex_2_digits: PASS
                                            n_number_infinity: PASS
                                           n_number_invalid+-: PASS
                               n_number_invalid-negative-real: PASS
                         n_number_invalid-utf-8-in-bigger-int: PASS
                           n_number_invalid-utf-8-in-exponent: PASS
                                n_number_invalid-utf-8-in-int: PASS
                                      n_number_minus_infinity: PASS
                    n_number_minus_sign_with_trailing_garbage: PASS
                                       n_number_minus_space_1: PASS
                          n_number_neg_int_starting_with_zero: PASS
                           n_number_neg_real_without_int_part: PASS
                             n_number_neg_with_garbage_at_end: PASS
                                n_number_real_garbage_after_e: PASS
                      n_number_real_with_invalid_utf8_after_e: PASS
                        n_number_real_without_fractional_part: PASS
                                   n_number_starting_with_dot: PASS
                                          n_number_with_alpha: PASS
                                     n_number_with_alpha_char: PASS
                                   n_number_with_leading_zero: PASS
                                           n_object_bad_value: PASS
                                         n_object_bracket_key: PASS
                              n_object_comma_instead_of_colon: PASS
                                        n_object_double_colon: PASS
                                               n_object_emoji: PASS
                                      n_object_garbage_at_end: PASS
                              n_object_key_with_single_quotes: PASS
    n_object_lone_continuation_byte_in_key_and_trailing_comma: PASS
                                       n_object_missing_colon: PASS
                                         n_object_missing_key: PASS
                                   n_object_missing_semicolon: PASS
                                       n_object_missing_value: PASS
                                            n_object_no-colon: PASS
                                      n_object_non_string_key: PASS
              n_object_non_string_key_but_huge_number_instead: PASS
                                  n_object_repeated_null_null: PASS
                             n_object_several_trailing_commas: PASS
                                        n_object_single_quote: PASS
                                      n_object_trailing_comma: PASS
                                    n_object_trailing_comment: PASS
                               n_object_trailing_comment_open: PASS
                         n_object_trailing_comment_slash_open: PASS
              n_object_trailing_comment_slash_open_incomplete: PASS
                                 n_object_two_commas_in_a_row: PASS
                                        n_object_unquoted_key: PASS
                                  n_object_unterminated-value: PASS
                                  n_object_with_single_string: PASS
                               n_object_with_trailing_garbage: PASS
                                               n_single_space: PASS
                             n_string_1_surrogate_then_escape: PASS
                           n_string_1_surrogate_then_escape_u: PASS
                          n_string_1_surrogate_then_escape_u1: PASS
                         n_string_1_surrogate_then_escape_u1x: PASS
                          n_string_accentuated_char_no_quotes: PASS
                                        n_string_backslash_00: PASS
                                            n_string_escape_x: PASS
                               n_string_escaped_backslash_bad: PASS
                               n_string_escaped_ctrl_char_tab: PASS
                                       n_string_escaped_emoji: PASS
                                   n_string_incomplete_escape: PASS
                        n_string_incomplete_escaped_character: PASS
                                n_string_incomplete_surrogate: PASS
                 n_string_incomplete_surrogate_escape_invalid: PASS
                             n_string_invalid-utf-8-in-escape: PASS
                               n_string_invalid_backslash_esc: PASS
                              n_string_invalid_unicode_escape: PASS
                           n_string_invalid_utf8_after_escape: PASS
                          n_string_leading_uescaped_thinspace: PASS
                           n_string_no_quotes_with_bad_escape: PASS
                                  n_string_single_doublequote: PASS
                                        n_string_single_quote: PASS
                      n_string_single_string_no_double_quotes: PASS
                               n_string_start_escape_unclosed: PASS
                                 n_string_unescaped_ctrl_char: PASS
                                   n_string_unescaped_newline: PASS
                                       n_string_unescaped_tab: PASS
                                    n_string_unicode_CapitalU: PASS
                               n_string_with_trailing_garbage: PASS
                            n_structure_100000_opening_arrays: PASS
                               n_structure_U+2060_word_joined: PASS
                                 n_structure_UTF8_BOM_no_data: PASS
                                  n_structure_angle_bracket_.: PASS
                               n_structure_angle_bracket_null: PASS
                           n_structure_array_trailing_garbage: PASS
                     n_structure_array_with_extra_array_close: PASS
                       n_structure_array_with_unclosed_string: PASS
                         n_structure_ascii-unicode-identifier: PASS
                                 n_structure_capitalized_True: PASS
                             n_structure_close_unopened_array: PASS
                   n_structure_comma_instead_of_closing_brace: PASS
                                     n_structure_double_array: PASS
                                        n_structure_end_array: PASS
                              n_structure_incomplete_UTF8_BOM: PASS
                               n_structure_lone-invalid-utf-8: PASS
                                n_structure_lone-open-bracket: PASS
                                          n_structure_no_data: PASS
                         n_structure_null-byte-outside-string: PASS
                     n_structure_number_with_trailing_garbage: PASS
                n_structure_object_followed_by_closing_object: PASS
                         n_structure_object_unclosed_no_value: PASS
                              n_structure_object_with_comment: PASS
                     n_structure_object_with_trailing_garbage: PASS
                            n_structure_open_array_apostrophe: PASS
                                 n_structure_open_array_comma: PASS
                                n_structure_open_array_object: PASS
                           n_structure_open_array_open_object: PASS
                           n_structure_open_array_open_string: PASS
                                n_structure_open_array_string: PASS
                                      n_structure_open_object: PASS
                          n_structure_open_object_close_array: PASS
                                n_structure_open_object_comma: PASS
                           n_structure_open_object_open_array: PASS
                          n_structure_open_object_open_string: PASS
              n_structure_open_object_string_with_apostrophes: PASS
                                        n_structure_open_open: PASS
                                    n_structure_single_eacute: PASS
                                      n_structure_single_star: PASS
                                       n_structure_trailing_#: PASS
                        n_structure_uescaped_LF_before_string: PASS
                                   n_structure_unclosed_array: PASS
                      n_structure_unclosed_array_partial_null: PASS
                  n_structure_unclosed_array_unfinished_false: PASS
                   n_structure_unclosed_array_unfinished_true: PASS
                                  n_structure_unclosed_object: PASS
                               n_structure_unicode-identifier: PASS
                    n_structure_whitespace_U+2060_word_joiner: PASS
                              n_structure_whitespace_formfeed: PASS

    # Implementation defined JSON tests
                                 i_number_double_huge_neg_exp: PARSES
                                            i_number_huge_exp: PARSES
                                    i_number_neg_int_huge_exp: PARSES
                                 i_number_pos_double_huge_exp: PARSES
                                   i_number_real_neg_overflow: PARSES
                                   i_number_real_pos_overflow: PARSES
                                      i_number_real_underflow: PARSES
                                     i_number_too_big_neg_int: PARSES
                                     i_number_too_big_pos_int: PARSES
                               i_number_very_big_negative_int: PARSES
                              i_object_key_lone_2nd_surrogate: DOES NOT PARSE
                       i_string_1st_surrogate_but_2nd_missing: DOES NOT PARSE
                     i_string_1st_valid_surrogate_2nd_invalid: DOES NOT PARSE
                                   i_string_UTF-16LE_with_BOM: DOES NOT PARSE
                              i_string_UTF-8_invalid_sequence: DOES NOT PARSE
                               i_string_UTF8_surrogate_U+D800: DOES NOT PARSE
               i_string_incomplete_surrogate_and_escape_valid: DOES NOT PARSE
                           i_string_incomplete_surrogate_pair: DOES NOT PARSE
                  i_string_incomplete_surrogates_escape_valid: DOES NOT PARSE
                            i_string_invalid_lonely_surrogate: DOES NOT PARSE
                                   i_string_invalid_surrogate: DOES NOT PARSE
                                       i_string_invalid_utf-8: DOES NOT PARSE
                         i_string_inverted_surrogates_U+1D11E: DOES NOT PARSE
                                         i_string_iso_latin_1: DOES NOT PARSE
                               i_string_lone_second_surrogate: DOES NOT PARSE
                         i_string_lone_utf8_continuation_byte: DOES NOT PARSE
                                i_string_not_in_unicode_range: DOES NOT PARSE
                           i_string_overlong_sequence_2_bytes: DOES NOT PARSE
                           i_string_overlong_sequence_6_bytes: DOES NOT PARSE
                      i_string_overlong_sequence_6_bytes_null: DOES NOT PARSE
                                     i_string_truncated-utf-8: DOES NOT PARSE
                                      i_string_utf16BE_no_BOM: DOES NOT PARSE
                                      i_string_utf16LE_no_BOM: DOES NOT PARSE
                                i_structure_500_nested_arrays: PARSES
                           i_structure_UTF-8_BOM_empty_object: DOES NOT PARSE |}]
;;
