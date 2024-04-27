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

exception InternalParseError of { message : string Lazy.t }

exception
  ParseError of
    { pos : int
    ; message : string
    }

module State : sig
  type t

  val create : string -> t

  (** Data access. *)

  val input_len : t -> int
  val unsafe_peek : t -> char
  val slice : t -> pos:int -> len:int -> string

  (** Position management. *)

  val pos : t -> int
  val unsafe_advance_pos : t -> by_:int -> unit
  val commit_pos : t -> unit
  val parse_error_is_protected : t -> bool

  val protect
    :  t
    -> (unit -> 'a)
    -> on_success:('a -> 'b)
    -> on_parse_error:(message:string Lazy.t -> 'b)
    -> 'b
end = struct
  type t =
    { buf : string
    ; mutable pos : int
    ; mutable protect_depth : int
    ; mutable last_commit : int
    }

  let create buf = { buf; pos = 0; protect_depth = 0; last_commit = 0 }
  let[@inline] input_len t = String.length t.buf
  let[@inline] unsafe_peek t = String.unsafe_get t.buf t.pos
  let[@inline] slice t ~pos ~len = String.sub t.buf ~pos ~len
  let[@inline] pos t = t.pos
  let[@inline] unsafe_advance_pos t ~by_ = t.pos <- t.pos + by_

  let[@inline] commit_pos t =
    assert (t.pos >= t.last_commit);
    t.last_commit <- t.pos
  ;;

  let[@cold] raise_cannot_backtrack ~current_pos ~backtrack_to ~last_commit ~message =
    raise
      (ParseError
         { pos = current_pos
         ; message =
             [%string
               "parser cannot backtrack to position %{backtrack_to#Int}, it committed at \
                position %{last_commit#Int}: %{force message}"]
         })
  ;;

  let[@inline] ensure_can_backtrack t ~backtrack_to ~message_on_error =
    match backtrack_to < t.last_commit with
    | true ->
      raise_cannot_backtrack
        ~current_pos:t.pos
        ~backtrack_to
        ~last_commit:t.last_commit
        ~message:message_on_error
    | false -> ()
  ;;

  let[@inline] parse_error_is_protected t = t.protect_depth <> 0

  let[@inline] protect t f ~on_success ~on_parse_error =
    let old_pos = t.pos in
    t.protect_depth <- t.protect_depth + 1;
    match f () with
    | exception InternalParseError { message } ->
      t.protect_depth <- t.protect_depth - 1;
      ensure_can_backtrack t ~backtrack_to:old_pos ~message_on_error:message;
      t.pos <- old_pos;
      on_parse_error ~message
    | value ->
      t.protect_depth <- t.protect_depth - 1;
      on_success value
  ;;
end

type 'a t = State.t -> 'a

let[@inline] run t buf ~require_input_entirely_consumed =
  let state = State.create buf in
  match t state with
  | exception InternalParseError { message } ->
    let backtrace = Backtrace.Exn.most_recent () in
    Exn.raise_with_original_backtrace
      (ParseError { pos = State.pos state; message = force message })
      backtrace
  | result ->
    let pos = State.pos state in
    (match require_input_entirely_consumed && pos < State.input_len state with
     | true -> raise (ParseError { pos; message = "not at end of input" })
     | false -> ());
    result
;;

let[@inline] return value =
  let[@inline] run (_ : State.t) = value in
  run
;;

let[@inline] map t ~f =
  let[@inline] run state =
    let value = t state in
    f value
  in
  run
;;

let[@inline] bind t ~f =
  let[@inline] run state =
    let value = t state in
    f value state
  in
  run
;;

module T0 = struct
  module M = struct
    include Monad.Make (struct
        type nonrec 'a t = 'a t

        let return = return
        let bind = bind
        let map = `Custom map
      end)

    module Let_syntax = struct
      include Let_syntax

      module Let_syntax = struct
        include Let_syntax

        let[@inline] both a b =
          let[@inline] run state =
            let a = a state in
            let b = b state in
            a, b
          in
          run
        ;;
      end
    end
  end

  include M

  let[@inline] parse_error message state =
    match State.parse_error_is_protected state with
    | true -> Exn.raise_without_backtrace (InternalParseError { message })
    | false -> raise (InternalParseError { message })
  ;;

  let[@inline] fail message =
    let[@inline] run state = parse_error (lazy message) state in
    run
  ;;

  let[@inline] fail' message =
    let[@inline] run state = parse_error message state in
    run
  ;;

  let[@inline] ( >> ) t1 t2 =
    let[@inline] run state =
      let _ = t1 state in
      let t2 = t2 state in
      t2
    in
    run
  ;;

  let[@inline] ( << ) t1 t2 =
    let[@inline] run state =
      let t1 = t1 state in
      let _ = t2 state in
      t1
    in
    run
  ;;

  let validate_repeated_args ~at_least ~at_most =
    (match at_least < 0 with
     | true ->
       raise_s [%message "Expected [at_least] argument to be at least 0" (at_least : int)]
     | false -> ());
    match at_most with
    | Some at_most when at_most < at_least ->
      raise_s
        [%message
          "Expected [at_most] argument to be at least [at_least]"
            (at_least : int)
            (at_most : int)]
    | _ -> ()
  ;;

  let skip_many, many =
    let[@inline] loop t ~at_least ~at_most ~init ~accum state =
      let so_far = ref 0 in
      let acc = ref init in
      while
        let start_pos = State.pos state in
        let continue =
          State.protect
            state
            (fun [@inline] () -> t state)
            ~on_parse_error:(fun [@inline] ~message ->
              match !so_far < at_least with
              | true ->
                parse_error
                  (lazy [%string "not enough instances of `parser`: %{force message}"])
                  state
              | false -> false)
            ~on_success:(fun [@inline] value ->
              Int.incr so_far;
              acc := accum !acc value;
              (* If we haven't made any progress through the input since last
                 time and we have enough values, stop here rather than keep
                 on going forever. *)
              match State.pos state = start_pos && !so_far >= at_least with
              | true -> false
              | false ->
                (match at_most with
                 | Some at_most when !so_far = at_most -> false
                 | _ -> true))
        in
        continue
      do
        ()
      done;
      !acc
    in
    let[@inline] many_gen t ~at_least ~at_most ~init ~accum ~finalize =
      validate_repeated_args ~at_least ~at_most;
      match at_least, at_most with
      | 0, Some 0 -> return init
      | _ ->
        let[@inline] run state =
          loop t ~at_least ~at_most ~init ~accum state |> finalize
        in
        run
    in
    let[@inline] skip_many t ~at_least ~at_most =
      many_gen
        t
        ~at_least
        ~at_most
        ~init:()
        ~accum:(fun [@inline] () _ -> ())
        ~finalize:Fn.id
    in
    let[@inline] many t ~at_least ~at_most =
      many_gen
        t
        ~at_least
        ~at_most
        ~init:[]
        ~accum:(fun [@inline] acc value -> value :: acc)
        ~finalize:List.rev
    in
    skip_many, many
  ;;

  let choices =
    let error_message = lazy "no matching choice" in
    let rec loop ts state =
      match ts with
      | [] -> parse_error error_message state
      | hd :: tl ->
        State.protect
          state
          (fun [@inline] () -> hd state)
          ~on_parse_error:(fun [@inline] ~message:_ -> loop tl state)
          ~on_success:Fn.id
    in
    fun [@inline] ts ->
      let[@inline] run state = loop ts state in
      run
  ;;

  let[@inline] sep_by t ~sep ~at_least ~at_most =
    validate_repeated_args ~at_least ~at_most;
    match at_least, at_most with
    | 0, Some 0 -> return []
    | _ ->
      let at_least_one =
        let%map.M hd = t
        and tl =
          many
            (sep >> t)
            ~at_least:(Int.max 0 (at_least - 1))
            ~at_most:
              (match at_most with
               | None -> None
               | Some at_most -> Some (Int.max 0 (at_most - 1)))
        in
        hd :: tl
      in
      (match at_least with
       | 0 -> choices [ at_least_one; return [] ]
       | _ -> at_least_one)
  ;;

  let[@inline] fix ~max_recursion_depth fn =
    let rec t =
      let cache = ref None in
      let recursion_depth = ref 0 in
      let[@inline] run state =
        match !recursion_depth > max_recursion_depth with
        | true ->
          recursion_depth := 0;
          raise
            (ParseError
               { pos = State.pos state; message = "maximum recursion depth exceeded" })
        | false ->
          Int.incr recursion_depth;
          (match
             match !cache with
             | Some parser -> parser state
             | None ->
               let parser = fn t in
               cache := Some parser;
               parser state
           with
           | exception (InternalParseError _ as exn) ->
             Int.decr recursion_depth;
             Exn.raise_with_original_backtrace exn (Backtrace.Exn.most_recent ())
           | value ->
             Int.decr recursion_depth;
             value)
      in
      run
    in
    t
  ;;

  let[@inline] either t1 t2 =
    let t1 =
      let%map.M t1 = t1 in
      (First t1 : _ Either.t)
    in
    let t2 =
      let%map.M t2 = t2 in
      (Second t2 : _ Either.t)
    in
    choices [ t1; t2 ]
  ;;

  let commit =
    let[@inline] run state = State.commit_pos state in
    run
  ;;

  let at_end_of_input =
    let[@inline] run state = State.input_len state = State.pos state in
    run
  ;;

  let end_of_input =
    let error_message = lazy "not at end of input" in
    let[@inline] run state =
      match State.pos state < State.input_len state with
      | true -> parse_error error_message state
      | false -> ()
    in
    run
  ;;

  let[@inline] consumed_bytes t =
    let[@inline] run state =
      let old_pos = State.pos state in
      let _ = t state in
      let new_pos = State.pos state in
      State.slice state ~pos:old_pos ~len:(new_pos - old_pos)
    in
    run
  ;;

  let[@inline] value_and_consumed_bytes t =
    let[@inline] run state =
      let old_pos = State.pos state in
      let value = t state in
      let new_pos = State.pos state in
      value, State.slice state ~pos:old_pos ~len:(new_pos - old_pos)
    in
    run
  ;;

  let validate_len_arg len =
    match len < 0 with
    | true -> raise_s [%message "Expected [len] argument to be at least 0" (len : int)]
    | false -> ()
  ;;

  let insufficient_input_error = lazy "insufficient input"

  let peek1 =
    let[@inline] run state =
      match State.pos state >= State.input_len state with
      | true -> None
      | false -> Some (State.unsafe_peek state)
    in
    run
  ;;

  let skip1 =
    let[@inline] run state =
      match State.pos state >= State.input_len state with
      | true -> parse_error insufficient_input_error state
      | false -> State.unsafe_advance_pos state ~by_:1
    in
    run
  ;;

  let take1 =
    let[@inline] run state =
      match State.pos state >= State.input_len state with
      | true -> parse_error insufficient_input_error state
      | false ->
        let value = State.unsafe_peek state in
        State.unsafe_advance_pos state ~by_:1;
        value
    in
    run
  ;;

  let[@inline] match1 value =
    let error_message = lazy [%string "expected character %{value#Char}"] in
    let[@inline] run state =
      match State.pos state >= State.input_len state with
      | true -> parse_error insufficient_input_error state
      | false ->
        let chr = State.unsafe_peek state in
        (match Char.( = ) chr value with
         | true ->
           State.unsafe_advance_pos state ~by_:1;
           chr
         | false -> parse_error error_message state)
    in
    run
  ;;

  let take1_cond =
    let error_message = lazy "character did not satisfy condition" in
    fun [@inline] f ->
      let[@inline] run state =
        match State.pos state >= State.input_len state with
        | true -> parse_error insufficient_input_error state
        | false ->
          let chr = State.unsafe_peek state in
          (match f chr with
           | true ->
             State.unsafe_advance_pos state ~by_:1;
             chr
           | false -> parse_error error_message state)
      in
      run
  ;;

  let[@inline] peek ~len =
    validate_len_arg len;
    let[@inline] run state =
      let pos = State.pos state in
      match pos + len > State.input_len state with
      | true -> None
      | false -> Some (State.slice state ~pos ~len)
    in
    run
  ;;

  let[@inline] skip ~len =
    validate_len_arg len;
    let[@inline] run state =
      let pos = State.pos state in
      match pos + len > State.input_len state with
      | true -> parse_error insufficient_input_error state
      | false -> State.unsafe_advance_pos state ~by_:len
    in
    run
  ;;

  let[@inline] take ~len = consumed_bytes (skip ~len)

  let match_ =
    let rec unsafe_equal_loop value ~value_pos ~value_len state ~error_message =
      match value_pos = value_len with
      | true -> value
      | false ->
        (match
           Char.( = ) (State.unsafe_peek state) (String.unsafe_get value value_pos)
         with
         | true ->
           State.unsafe_advance_pos state ~by_:1;
           unsafe_equal_loop
             value
             ~value_pos:(value_pos + 1)
             ~value_len
             state
             ~error_message
         | false -> parse_error error_message state)
    in
    fun [@inline] value ->
      let error_message = lazy [%string "expected string %{value}"] in
      let value_len = String.length value in
      let[@inline] run state =
        let pos = State.pos state in
        match pos + value_len > State.input_len state with
        | true -> parse_error insufficient_input_error state
        | false -> unsafe_equal_loop value ~value_pos:0 ~value_len state ~error_message
      in
      run
  ;;

  let skip_while =
    let[@inline] loop ~f ~at_least ~at_most state ~error_expected_at_least =
      let start_pos = State.pos state in
      let input_len = State.input_len state in
      while
        let pos = State.pos state in
        let bounds_ok = pos < input_len in
        let at_most_ok =
          match at_most with
          | None -> true
          | Some at_most -> pos - start_pos < at_most
        in
        let continue =
          match bounds_ok && at_most_ok && f (State.unsafe_peek state) with
          | true ->
            State.unsafe_advance_pos state ~by_:1;
            true
          | false ->
            (match pos - start_pos < at_least with
             | true -> parse_error error_expected_at_least state
             | false -> false)
        in
        continue
      do
        ()
      done
    in
    fun [@inline] ~f ~at_least ~at_most ->
      validate_repeated_args ~at_least ~at_most;
      let error_expected_at_least =
        lazy [%string "expected at least %{at_least#Int} matching chars"]
      in
      let[@inline] run state =
        loop ~f ~at_least ~at_most state ~error_expected_at_least
      in
      run
  ;;

  let[@inline] take_while ~f ~at_least ~at_most =
    consumed_bytes (skip_while ~f ~at_least ~at_most)
  ;;

  let fold =
    let rec loop acc ~f state =
      let pos = State.pos state in
      match pos > State.input_len state with
      | true -> parse_error insufficient_input_error state
      | false ->
        let peek =
          match pos = State.input_len state with
          | true -> `Eof
          | false -> `Char (State.unsafe_peek state)
        in
        (match f acc ~peek with
         | `Fail message -> parse_error message state
         | `Return acc -> acc
         | `Advance acc ->
           State.unsafe_advance_pos state ~by_:1;
           loop acc ~f state)
    in
    fun [@inline] ~init ~f ->
      let[@inline] run state = loop init ~f state in
      run
  ;;

  let foldn =
    let rec loop ~left acc ~f state =
      match left with
      | 0 -> acc
      | _ ->
        (match f acc (State.unsafe_peek state) with
         | Error message -> parse_error message state
         | Ok acc ->
           State.unsafe_advance_pos state ~by_:1;
           loop ~left:(left - 1) acc ~f state)
    in
    fun [@inline] ~n ~init ~f ->
      let[@inline] run state =
        match State.pos state + n > State.input_len state with
        | true -> parse_error insufficient_input_error state
        | false -> loop ~left:n init ~f state
      in
      run
  ;;

  let[@inline] is_whitespace = function
    | ' ' | '\t' | '\n' | '\r' -> true
    | _ -> false
  ;;

  let whitespace0 = skip_while ~f:is_whitespace ~at_least:0 ~at_most:None
  let whitespace = skip_while ~f:is_whitespace ~at_least:1 ~at_most:None

  let non_negative_integer =
    let%map.M digits =
      take_while
        ~f:(function
          | '0' .. '9' -> true
          | _ -> false)
        ~at_least:1
        ~at_most:None
    in
    Int.of_string digits
  ;;

  let integer =
    let%map.M minus = take_while ~f:(Char.( = ) '-') ~at_least:0 ~at_most:(Some 1)
    and value = non_negative_integer in
    match String.( = ) minus "" with
    | true -> value
    | false -> -value
  ;;

  let buffered_output ?(initial_capacity = 8192) t =
    let buf = Buffer.create initial_capacity in
    let emit char = Buffer.add_char buf char in
    let t : _ t = t ~emit in
    let[@inline] run state =
      Buffer.clear buf;
      t state;
      Buffer.contents buf
    in
    run
  ;;

  module Utf8_encoded = struct
    module M = struct
      type t = int [@@deriving compare, hash]

      let[@inline] encoded_data t ~ascii ~two_byte ~three_byte ~four_byte =
        match t <= 0xff with
        | true -> ascii t
        | false ->
          (match t <= 0xffff with
           | true ->
             let c1 = t lsr 8 in
             let c2 = t land 0xff in
             two_byte c1 c2
           | false ->
             (match t <= 0xffffff with
              | true ->
                let c1 = t lsr 16 in
                let c2 = (t lsr 8) land 0xff in
                let c3 = t land 0xff in
                three_byte c1 c2 c3
              | false ->
                let c1 = t lsr 24 in
                let c2 = (t lsr 16) land 0xff in
                let c3 = (t lsr 8) land 0xff in
                let c4 = t land 0xff in
                four_byte c1 c2 c3 c4))
      ;;

      let to_string t =
        encoded_data
          t
          ~ascii:(Fn.compose String.of_char Char.unsafe_of_int)
          ~two_byte:(fun c1 c2 ->
            let c1 = Char.unsafe_of_int c1 in
            let c2 = Char.unsafe_of_int c2 in
            String.of_char_list [ c1; c2 ])
          ~three_byte:(fun c1 c2 c3 ->
            let c1 = Char.unsafe_of_int c1 in
            let c2 = Char.unsafe_of_int c2 in
            let c3 = Char.unsafe_of_int c3 in
            String.of_char_list [ c1; c2; c3 ])
          ~four_byte:(fun c1 c2 c3 c4 ->
            let c1 = Char.unsafe_of_int c1 in
            let c2 = Char.unsafe_of_int c2 in
            let c3 = Char.unsafe_of_int c3 in
            let c4 = Char.unsafe_of_int c4 in
            String.of_char_list [ c1; c2; c3; c4 ])
      ;;

      let sexp_of_t = Fn.compose [%sexp_of: string] to_string
    end

    include M
    include Comparable.Make (M)

    let[@inline] to_code t =
      encoded_data
        t
        ~ascii:Fn.id
        ~two_byte:(fun [@inline] c1 c2 ->
          ((c1 land 0b00011111) lsl 6) lor (c2 land 0b00111111))
        ~three_byte:(fun [@inline] c1 c2 c3 ->
          ((c1 land 0b00001111) lsl 12)
          lor ((c2 land 0b00111111) lsl 6)
          lor (c3 land 0b00111111))
        ~four_byte:(fun [@inline] c1 c2 c3 c4 ->
          ((c1 land 0b00000111) lsl 18)
          lor ((c2 land 0b00111111) lsl 12)
          lor ((c3 land 0b00111111) lsl 6)
          lor (c4 land 0b00111111))
    ;;

    let[@cold] exn_invalid_code code =
      raise_s [%message "Invalid Unicode code point" (code : int)]
    ;;

    let[@inline] of_code_exn code =
      match code <= 127 with
      | true ->
        (match code >= 0 with
         | true -> code
         | false -> raise @@ exn_invalid_code code)
      | false ->
        (match code <= 0x7ff with
         | true -> 0xc080 lor ((code lsl 2) land 0x1f00) lor (code land 0x3f)
         | false ->
           (match code <= 0xffff with
            | true ->
              (match code < 0xd800 || code > 0xdfff with
               | true ->
                 0xe08080
                 lor ((code lsl 4) land 0x0f0000)
                 lor ((code lsl 2) land 0x3f00)
                 lor (code land 0x3f)
               | false -> raise @@ exn_invalid_code code)
            | false ->
              (match code <= 0x10ffff with
               | true ->
                 0xf0808080
                 lor ((code lsl 6) land 0x07000000)
                 lor ((code lsl 4) land 0x3f0000)
                 lor ((code lsl 2) land 0x3f00)
                 lor (code land 0x3f)
               | false -> raise @@ exn_invalid_code code)))
    ;;

    let[@inline] unchecked_of_code code =
      match code <= 127 with
      | true -> code
      | false ->
        (match code <= 0x7ff with
         | true -> 0xc080 lor ((code lsl 2) land 0x1f00) lor (code land 0x3f)
         | false ->
           (match code <= 0xffff with
            | true ->
              0xe08080
              lor ((code lsl 4) land 0x0f0000)
              lor ((code lsl 2) land 0x3f00)
              lor (code land 0x3f)
            | false ->
              0xf0808080
              lor ((code lsl 6) land 0x07000000)
              lor ((code lsl 4) land 0x3f0000)
              lor ((code lsl 2) land 0x3f00)
              lor (code land 0x3f)))
    ;;

    let replacement_character = of_code_exn 0xfffd

    let[@inline] is_valid_byte_2_4 = function
      | '\128' .. '\191' -> true
      | _ -> false
    ;;

    let[@inline] parse_exn
      ~first_byte
      ~next_byte_exists
      ~unsafe_peek
      ~unsafe_advance_byte
      ~parse_error
      =
      let[@inline] with_next_char ~is_valid f =
        match next_byte_exists () with
        | false -> parse_error ()
        | true ->
          let chr = unsafe_peek () in
          (match is_valid chr with
           | true ->
             unsafe_advance_byte ();
             f chr
           | false -> parse_error ())
      in
      match first_byte with
      | '\000' .. '\127' as c -> Char.to_int c
      | '\128' .. '\193' | '\245' .. '\255' -> parse_error ()
      | '\194' .. '\223' as c1 ->
        with_next_char ~is_valid:is_valid_byte_2_4 (fun [@inline] c2 ->
          let c1 = Char.to_int c1 in
          let c2 = Char.to_int c2 in
          (c1 lsl 8) lor c2)
      | '\224' .. '\239' as c1 ->
        with_next_char
          ~is_valid:
            (match c1 with
             | '\224' ->
               (function
                 | '\160' .. '\191' -> true
                 | _ -> false)
             | '\237' ->
               (function
                 | '\128' .. '\159' -> true
                 | _ -> false)
             | _ -> is_valid_byte_2_4)
          (fun [@inline] c2 ->
            with_next_char ~is_valid:is_valid_byte_2_4 (fun [@inline] c3 ->
              let c1 = Char.to_int c1 in
              let c2 = Char.to_int c2 in
              let c3 = Char.to_int c3 in
              (c1 lsl 16) lor (c2 lsl 8) lor c3))
      | '\240' .. '\244' as c1 ->
        with_next_char
          ~is_valid:
            (match c1 with
             | '\240' ->
               (function
                 | '\144' .. '\191' -> true
                 | _ -> false)
             | '\244' ->
               (function
                 | '\128' .. '\143' -> true
                 | _ -> false)
             | _ -> is_valid_byte_2_4)
          (fun [@inline] c2 ->
            with_next_char ~is_valid:is_valid_byte_2_4 (fun [@inline] c3 ->
              with_next_char ~is_valid:is_valid_byte_2_4 (fun [@inline] c4 ->
                let c1 = Char.to_int c1 in
                let c2 = Char.to_int c2 in
                let c3 = Char.to_int c3 in
                let c4 = Char.to_int c4 in
                (c1 lsl 24) lor (c2 lsl 16) lor (c3 lsl 8) lor c4)))
    ;;

    let[@inline] emit_encoded_data t ~emit =
      let[@inline] emit' value = emit (Char.unsafe_of_int value) in
      encoded_data
        t
        ~ascii:(fun [@inline] byte -> emit' byte)
        ~two_byte:(fun [@inline] c1 c2 ->
          emit' c1;
          emit' c2)
        ~three_byte:(fun [@inline] c1 c2 c3 ->
          emit' c1;
          emit' c2;
          emit' c3)
        ~four_byte:(fun [@inline] c1 c2 c3 c4 ->
          emit' c1;
          emit' c2;
          emit' c3;
          emit' c4)
    ;;
  end

  let take1_utf8 =
    let[@inline] run state =
      let first_byte = take1 state in
      Utf8_encoded.parse_exn
        ~first_byte
        ~next_byte_exists:(fun [@inline] () -> State.pos state < State.input_len state)
        ~unsafe_peek:(fun [@inline] () -> State.unsafe_peek state)
        ~unsafe_advance_byte:(fun [@inline] () -> State.unsafe_advance_pos state ~by_:1)
        ~parse_error:(fun [@inline] () -> Utf8_encoded.replacement_character)
    in
    run
  ;;

  let take1_strict_utf8 =
    let error_message = lazy "Unexpected UTF8 byte sequence" in
    let[@inline] run state =
      let first_byte = take1 state in
      Utf8_encoded.parse_exn
        ~first_byte
        ~next_byte_exists:(fun [@inline] () -> State.pos state < State.input_len state)
        ~unsafe_peek:(fun [@inline] () -> State.unsafe_peek state)
        ~unsafe_advance_byte:(fun [@inline] () -> State.unsafe_advance_pos state ~by_:1)
        ~parse_error:(fun [@inline] () -> parse_error error_message state)
    in
    run
  ;;

  let skip1_strict_utf8 =
    let[@inline] run state =
      let (_ : Utf8_encoded.t) = take1_strict_utf8 state in
      ()
    in
    run
  ;;
end

module T = struct
  include T0

  module Let_syntax = struct
    include Let_syntax

    module Let_syntax = struct
      include Let_syntax
      module Open_on_rhs = T0
    end
  end
end

include T

module With_let_syntax = struct
  exception
    ParseError of
      { pos : int
      ; message : string
      }

  let run = run

  include T
  include Let_syntax
end
