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
  module M = Monad.Make (struct
      type nonrec 'a t = 'a t

      let return = return
      let bind = bind
      let map = `Custom map
    end)

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
    let%map.M _ = t1
    and t2 = t2 in
    t2
  ;;

  let[@inline] ( << ) t1 t2 =
    let%map.M t1 = t1
    and _ = t2 in
    t1
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

  let many =
    let rec loop t ~at_least ~at_most state acc =
      let start_pos = State.pos state in
      State.protect
        state
        (fun [@inline] () -> t state)
        ~on_parse_error:(fun [@inline] ~message ->
          match Queue.length acc < at_least with
          | true ->
            parse_error
              (lazy [%string "not enough instances of `parser`: %{force message}"])
              state
          | false -> Queue.to_list acc)
        ~on_success:(fun [@inline] value ->
          Queue.enqueue acc value;
          (* If we haven't made any progress through the input since last
             time and we have enough values, stop here rather than keep
             on going forever. *)
          match State.pos state = start_pos && Queue.length acc >= at_least with
          | true -> Queue.to_list acc
          | false ->
            (match at_most with
             | Some at_most when Queue.length acc = at_most -> Queue.to_list acc
             | _ -> loop t ~at_least ~at_most state acc))
    in
    fun [@inline] t ~at_least ~at_most ->
      validate_repeated_args ~at_least ~at_most;
      match at_least, at_most with
      | 0, Some 0 -> return []
      | _ ->
        let[@inline] run state = loop t ~at_least ~at_most state (Queue.create ()) in
        run
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

  let[@inline] fix fn =
    let rec t = fun [@inline] state -> fn t state in
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

  let[@inline] match_ value =
    let error_message = lazy [%string "expected string %{value}"] in
    let value_len = String.length value in
    let[@inline] run state =
      let pos = State.pos state in
      match pos + value_len > State.input_len state with
      | true -> parse_error insufficient_input_error state
      | false ->
        let slice = State.slice state ~pos ~len:value_len in
        (match String.( = ) slice value with
         | true ->
           State.unsafe_advance_pos state ~by_:value_len;
           value
         | false -> parse_error error_message state)
    in
    run
  ;;

  let skip_while =
    let rec loop
      ~f
      ~at_least
      ~at_most
      state
      ~input_len
      ~start_pos
      ~error_expected_at_least
      =
      let pos = State.pos state in
      let bounds_ok = pos < input_len in
      let at_most_ok =
        match at_most with
        | None -> true
        | Some at_most -> pos - start_pos < at_most
      in
      match bounds_ok && at_most_ok && f (State.unsafe_peek state) with
      | true ->
        State.unsafe_advance_pos state ~by_:1;
        loop ~f ~at_least ~at_most state ~input_len ~start_pos ~error_expected_at_least
      | false ->
        (match pos - start_pos < at_least with
         | true -> parse_error error_expected_at_least state
         | false -> ())
    in
    fun [@inline] ~f ~at_least ~at_most ->
      validate_repeated_args ~at_least ~at_most;
      let error_expected_at_least =
        lazy [%string "expected at least %{at_least#Int} matching chars"]
      in
      let[@inline] run state =
        let start_pos = State.pos state in
        loop
          ~f
          ~at_least
          ~at_most
          state
          ~input_len:(State.input_len state)
          ~start_pos
          ~error_expected_at_least
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
