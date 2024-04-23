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
  InternalParseError of
    { pos : int
    ; message : string Lazy.t
    }

exception
  ParseError of
    { pos : int
    ; message : string
    }

module State : sig
  type t

  val create : string -> t
  val input_len : t -> int
  val get : t -> int -> char
  val slice : t -> pos:int -> len:int -> string

  val ensure_can_backtrack
    :  t
    -> current_pos:int
    -> backtrack_to:int
    -> message_on_error:string Lazy.t
    -> unit

  val commit_position : t -> int -> unit
end = struct
  type t =
    { buf : string
    ; mutable committed_bytes : int
    }

  let create buf = { buf; committed_bytes = 0 }
  let input_len t = String.length t.buf
  let get t pos = String.get t.buf pos
  let slice t ~pos ~len = String.sub t.buf ~pos ~len

  let[@cold] raise_cannot_backtrack ~current_pos ~backtrack_to ~committed_bytes ~message =
    raise
      (ParseError
         { pos = current_pos
         ; message =
             [%string
               "parser cannot backtrack to position %{backtrack_to#Int}, it committed at \
                position %{committed_bytes#Int}: %{force message}"]
         })
  ;;

  let[@inline] ensure_can_backtrack t ~current_pos ~backtrack_to ~message_on_error =
    match backtrack_to < t.committed_bytes with
    | true ->
      raise_cannot_backtrack
        ~current_pos
        ~backtrack_to
        ~committed_bytes:t.committed_bytes
        ~message:message_on_error
    | false -> ()
  ;;

  let commit_position t pos =
    (match pos < t.committed_bytes with
     | true ->
       raise_s
         [%message
           "Parser input already committed beyond position"
             ~committed_bytes:(t.committed_bytes : int)
             (pos : int)]
     | false -> ());
    t.committed_bytes <- pos
  ;;
end

type 'a t = State.t -> pos:int -> int * 'a

let[@inline] run t buf ~require_input_entirely_consumed =
  let state = State.create buf in
  match t state ~pos:0 with
  | exception InternalParseError { pos; message } ->
    let backtrace = Backtrace.Exn.most_recent () in
    Exn.raise_with_original_backtrace
      (ParseError { pos; message = force message })
      backtrace
  | pos, result ->
    (match require_input_entirely_consumed && pos < State.input_len state with
     | true -> raise (ParseError { pos; message = "not at end of input" })
     | false -> ());
    result
;;

let[@inline] return value =
  let[@inline] run (_ : State.t) ~pos = pos, value in
  run
;;

let[@inline] map t ~f =
  let[@inline] run state ~pos =
    let pos, value = t state ~pos in
    pos, f value
  in
  run
;;

let[@inline] bind t ~f =
  let[@inline] run state ~pos =
    let pos, value = t state ~pos in
    f value state ~pos
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

  let[@inline] parse_error message ~pos = raise (InternalParseError { pos; message })

  let[@inline] fail message =
    let[@inline] run (_ : State.t) ~pos = parse_error (lazy message) ~pos in
    run
  ;;

  let[@inline] fail' message =
    let[@inline] run (_ : State.t) ~pos = parse_error message ~pos in
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
    let rec loop t ~at_least ~at_most state ~pos acc =
      match t state ~pos with
      | exception InternalParseError { pos = current_pos; message } ->
        State.ensure_can_backtrack
          state
          ~current_pos
          ~backtrack_to:pos
          ~message_on_error:message;
        (match Queue.length acc < at_least with
         | true ->
           parse_error
             (lazy [%string "not enough instances of `parser`: %{force message}"])
             ~pos
         | false -> pos, Queue.to_list acc)
      | new_pos, value ->
        Queue.enqueue acc value;
        (* If we haven't made any progress through the input since last
           time and we have enough values, stop here rather than keep
           on going forever. *)
        (match pos = new_pos && Queue.length acc >= at_least with
         | true -> new_pos, Queue.to_list acc
         | false ->
           (match at_most with
            | Some at_most when Queue.length acc = at_most -> new_pos, Queue.to_list acc
            | _ -> loop t ~at_least ~at_most state ~pos:new_pos acc))
    in
    fun [@inline] t ~at_least ~at_most ->
      validate_repeated_args ~at_least ~at_most;
      match at_least, at_most with
      | 0, Some 0 -> return []
      | _ ->
        let[@inline] run state ~pos =
          loop t ~at_least ~at_most state ~pos (Queue.create ())
        in
        run
  ;;

  let choices =
    let error_message = lazy "no matching choice" in
    let rec loop ts state ~pos =
      match ts with
      | [] -> parse_error error_message ~pos
      | hd :: tl ->
        (match hd state ~pos with
         | exception InternalParseError { pos = current_pos; message } ->
           State.ensure_can_backtrack
             state
             ~current_pos
             ~backtrack_to:pos
             ~message_on_error:message;
           loop tl state ~pos
         | new_pos, value -> new_pos, value)
    in
    fun [@inline] ts ->
      let[@inline] run state ~pos = loop ts state ~pos in
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
    let rec t = fun [@inline] state ~pos -> fn t state ~pos in
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
    let[@inline] run state ~pos =
      State.commit_position state pos;
      pos, ()
    in
    run
  ;;

  let at_end_of_input =
    let[@inline] run state ~pos = pos, State.input_len state = pos in
    run
  ;;

  let end_of_input =
    let error_message = lazy "not at end of input" in
    let[@inline] run state ~pos =
      match pos < State.input_len state with
      | true -> parse_error error_message ~pos
      | false -> pos, ()
    in
    run
  ;;

  let[@inline] consumed_bytes t =
    let[@inline] run state ~pos =
      let new_pos, _ = t state ~pos in
      new_pos, State.slice state ~pos ~len:(new_pos - pos)
    in
    run
  ;;

  let[@inline] value_and_consumed_bytes t =
    let[@inline] run state ~pos =
      let new_pos, value = t state ~pos in
      new_pos, (value, State.slice state ~pos ~len:(new_pos - pos))
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
    let[@inline] run state ~pos =
      let value =
        match pos >= State.input_len state with
        | true -> None
        | false -> Some (State.get state pos)
      in
      pos, value
    in
    run
  ;;

  let skip1 =
    let[@inline] run state ~pos =
      let value =
        match pos >= State.input_len state with
        | true -> parse_error insufficient_input_error ~pos
        | false -> ()
      in
      pos + 1, value
    in
    run
  ;;

  let take1 =
    let[@inline] run state ~pos =
      let value =
        match pos >= State.input_len state with
        | true -> parse_error insufficient_input_error ~pos
        | false -> State.get state pos
      in
      pos + 1, value
    in
    run
  ;;

  let[@inline] match1 value =
    let error_message = lazy [%string "expected character %{value#Char}"] in
    let[@inline] run state ~pos =
      match pos >= State.input_len state with
      | true -> parse_error insufficient_input_error ~pos
      | false ->
        let chr = State.get state pos in
        (match Char.( = ) chr value with
         | true -> pos + 1, value
         | false -> parse_error error_message ~pos)
    in
    run
  ;;

  let take1_cond =
    let error_message = lazy "character did not satisfy condition" in
    fun [@inline] f ->
      let[@inline] run state ~pos =
        match pos >= State.input_len state with
        | true -> parse_error insufficient_input_error ~pos
        | false ->
          let chr = State.get state pos in
          (match f chr with
           | true -> pos + 1, chr
           | false -> parse_error error_message ~pos)
      in
      run
  ;;

  let[@inline] peek ~len =
    validate_len_arg len;
    let[@inline] run state ~pos =
      let value =
        match pos + len > State.input_len state with
        | true -> None
        | false -> Some (State.slice state ~pos ~len)
      in
      pos, value
    in
    run
  ;;

  let[@inline] skip ~len =
    validate_len_arg len;
    let[@inline] run state ~pos =
      let value =
        match pos + len > State.input_len state with
        | true -> parse_error insufficient_input_error ~pos
        | false -> ()
      in
      pos + len, value
    in
    run
  ;;

  let[@inline] take ~len = consumed_bytes (skip ~len)

  let[@inline] match_ value =
    let error_message = lazy [%string "expected string %{value}"] in
    let[@inline] run state ~pos =
      let value_len = String.length value in
      match pos + value_len > State.input_len state with
      | true -> parse_error insufficient_input_error ~pos
      | false ->
        let slice = State.slice state ~pos ~len:value_len in
        (match String.( = ) slice value with
         | true -> pos + value_len, value
         | false -> parse_error error_message ~pos)
    in
    run
  ;;

  let skip_while =
    let rec loop ~f ~at_least ~at_most state ~pos ~input_len acc ~error_expected_at_least =
      let bounds_ok = pos + acc < input_len in
      let at_most_ok =
        match at_most with
        | None -> true
        | Some at_most -> acc < at_most
      in
      match bounds_ok && at_most_ok && f (State.get state (pos + acc)) with
      | true ->
        loop
          ~f
          ~at_least
          ~at_most
          state
          ~pos
          ~input_len
          (acc + 1)
          ~error_expected_at_least
      | false ->
        (match acc < at_least with
         | true -> parse_error error_expected_at_least ~pos:(pos + acc)
         | false -> pos + acc, ())
    in
    fun [@inline] ~f ~at_least ~at_most ->
      validate_repeated_args ~at_least ~at_most;
      let error_expected_at_least =
        lazy [%string "expected at least %{at_least#Int} matching chars"]
      in
      let[@inline] run state ~pos =
        loop
          ~f
          ~at_least
          ~at_most
          state
          ~pos
          ~input_len:(State.input_len state)
          0
          ~error_expected_at_least
      in
      run
  ;;

  let[@inline] take_while ~f ~at_least ~at_most =
    consumed_bytes (skip_while ~f ~at_least ~at_most)
  ;;

  let fold =
    let rec loop acc ~f state ~pos =
      match pos > State.input_len state with
      | true -> parse_error insufficient_input_error ~pos
      | false ->
        let peek =
          match pos = State.input_len state with
          | true -> `Eof
          | false -> `Char (State.get state pos)
        in
        (match f acc ~peek with
         | `Fail message -> parse_error message ~pos
         | `Return acc -> pos, acc
         | `Advance acc -> loop acc ~f state ~pos:(pos + 1))
    in
    fun [@inline] ~init ~f ->
      let[@inline] run state ~pos = loop init ~f state ~pos in
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
    let[@inline] run state ~pos =
      Buffer.clear buf;
      let pos, () = t state ~pos in
      let output = Buffer.contents buf in
      pos, output
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
