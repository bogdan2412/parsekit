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

exception
  CannotBacktrack of
    { pos : int
    ; message : string
    }

module Input : sig
  type t

  val create : string -> t
  val length : t -> int
  val get : t -> int -> char
  val slice : t -> pos:int -> len:int -> string

  val ensure_can_backtrack
    :  t
    -> current_pos:int
    -> backtrack_to:int
    -> message_on_error:string
    -> unit

  val commit_position : t -> int -> unit
end = struct
  type t =
    { buf : string
    ; mutable committed_bytes : int
    }

  let create buf = { buf; committed_bytes = 0 }
  let length t = String.length t.buf
  let get t pos = String.get t.buf pos
  let slice t ~pos ~len = String.sub t.buf ~pos ~len

  let[@cold] raise_cannot_backtrack ~current_pos ~backtrack_to ~committed_bytes ~message =
    raise
      (CannotBacktrack
         { pos = current_pos
         ; message =
             [%string
               "parser cannot backtrack to position %{backtrack_to#Int}, it committed at \
                position %{committed_bytes#Int}: %{message}"]
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

type 'a t = Input.t -> pos:int -> int * 'a

let[@inline] run t buf ~require_input_entirely_consumed =
  let input = Input.create buf in
  match t input ~pos:0 with
  | exception CannotBacktrack { pos; message } ->
    let backtrace = Backtrace.Exn.most_recent () in
    Exn.raise_with_original_backtrace (ParseError { pos; message }) backtrace
  | pos, result ->
    (match require_input_entirely_consumed && pos < Input.length input with
     | true -> raise (ParseError { pos; message = "not at end of input" })
     | false -> ());
    result
;;

let[@inline] return value =
  let[@inline] run (_ : Input.t) ~pos = pos, value in
  run
;;

let[@inline] fail message =
  let[@inline] run (_ : Input.t) ~pos = raise (ParseError { pos; message }) in
  run
;;

let[@inline] map t ~f =
  let[@inline] run input ~pos =
    let pos, value = t input ~pos in
    pos, f value
  in
  run
;;

let[@inline] bind t ~f =
  let[@inline] run input ~pos =
    let pos, value = t input ~pos in
    f value input ~pos
  in
  run
;;

include Monad.Make (struct
    type nonrec 'a t = 'a t

    let return = return
    let bind = bind
    let map = `Custom map
  end)

open Let_syntax

let[@inline] ( >> ) t1 t2 =
  let%map _ = t1
  and t2 = t2 in
  t2
;;

let[@inline] ( << ) t1 t2 =
  let%map t1 = t1
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
  let rec loop t ~at_least ~at_most input ~pos acc =
    match t input ~pos with
    | exception ParseError { pos = current_pos; message } ->
      Input.ensure_can_backtrack
        input
        ~current_pos
        ~backtrack_to:pos
        ~message_on_error:message;
      (match Queue.length acc < at_least with
       | true ->
         raise
           (ParseError
              { pos; message = [%string "not enough instances of `parser`: %{message}"] })
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
          | _ -> loop t ~at_least ~at_most input ~pos:new_pos acc))
  in
  fun [@inline] t ~at_least ~at_most ->
    validate_repeated_args ~at_least ~at_most;
    match at_least, at_most with
    | 0, Some 0 -> return []
    | _ ->
      let[@inline] run input ~pos =
        loop t ~at_least ~at_most input ~pos (Queue.create ())
      in
      run
;;

let choices =
  let rec loop ts input ~pos =
    match ts with
    | [] -> raise (ParseError { pos; message = "no matching choice" })
    | hd :: tl ->
      (match hd input ~pos with
       | exception ParseError { pos = current_pos; message } ->
         Input.ensure_can_backtrack
           input
           ~current_pos
           ~backtrack_to:pos
           ~message_on_error:message;
         loop tl input ~pos
       | new_pos, value -> new_pos, value)
  in
  fun [@inline] ts ->
    let[@inline] run input ~pos = loop ts input ~pos in
    run
;;

let[@inline] sep_by t ~sep ~at_least ~at_most =
  validate_repeated_args ~at_least ~at_most;
  match at_least, at_most with
  | 0, Some 0 -> return []
  | _ ->
    let at_least_one =
      let%map hd = t
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
  let rec t = fun [@inline] input ~pos -> fn t input ~pos in
  t
;;

let[@inline] either t1 t2 =
  let t1 =
    let%map t1 = t1 in
    (First t1 : _ Either.t)
  in
  let t2 =
    let%map t2 = t2 in
    (Second t2 : _ Either.t)
  in
  choices [ t1; t2 ]
;;

let commit =
  let[@inline] run input ~pos =
    Input.commit_position input pos;
    pos, ()
  in
  run
;;

let at_end_of_input =
  let[@inline] run input ~pos = pos, Input.length input = pos in
  run
;;

let end_of_input =
  let[@inline] run input ~pos =
    match pos < Input.length input with
    | true -> raise (ParseError { pos; message = "not at end of input" })
    | false -> pos, ()
  in
  run
;;

let[@inline] consumed_bytes t =
  let[@inline] run input ~pos =
    let new_pos, _ = t input ~pos in
    new_pos, Input.slice input ~pos ~len:(new_pos - pos)
  in
  run
;;

let[@inline] value_and_consumed_bytes t =
  let[@inline] run input ~pos =
    let new_pos, value = t input ~pos in
    new_pos, (value, Input.slice input ~pos ~len:(new_pos - pos))
  in
  run
;;

let validate_len_arg len =
  match len < 0 with
  | true -> raise_s [%message "Expected [len] argument to be at least 0" (len : int)]
  | false -> ()
;;

let[@inline] peek ~len =
  validate_len_arg len;
  let[@inline] run input ~pos =
    let value =
      match pos + len > Input.length input with
      | true -> None
      | false -> Some (Input.slice input ~pos ~len)
    in
    pos, value
  in
  run
;;

let[@inline] take ~len =
  validate_len_arg len;
  let[@inline] run input ~pos =
    let value =
      match pos + len > Input.length input with
      | true -> raise (ParseError { pos; message = "insufficient input" })
      | false -> Input.slice input ~pos ~len
    in
    pos + len, value
  in
  run
;;

let[@inline] match_ value =
  let[@inline] run input ~pos =
    let value_len = String.length value in
    match pos + value_len > Input.length input with
    | true -> raise (ParseError { pos; message = "insufficient input" })
    | false ->
      let slice = Input.slice input ~pos ~len:value_len in
      (match String.( = ) slice value with
       | true -> pos + value_len, value
       | false ->
         raise (ParseError { pos; message = [%string "expected string %{value}"] }))
  in
  run
;;

let take_while =
  let rec loop ~f ~at_least ~at_most input ~pos ~input_len acc =
    let bounds_ok = pos + acc < input_len in
    let at_most_ok =
      match at_most with
      | None -> true
      | Some at_most -> acc < at_most
    in
    match bounds_ok && at_most_ok && f (Input.get input (pos + acc)) with
    | true -> loop ~f ~at_least ~at_most input ~pos ~input_len (acc + 1)
    | false ->
      (match acc < at_least with
       | true ->
         raise
           (ParseError
              { pos = pos + acc
              ; message = [%string "expected at least %{at_least#Int} matching chars"]
              })
       | false -> pos + acc, Input.slice input ~pos ~len:acc)
  in
  fun [@inline] ~f ~at_least ~at_most ->
    validate_repeated_args ~at_least ~at_most;
    let[@inline] run input ~pos =
      loop ~f ~at_least ~at_most input ~pos ~input_len:(Input.length input) 0
    in
    run
;;

let[@inline] is_whitespace = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false
;;

let whitespace0 = take_while ~f:is_whitespace ~at_least:0 ~at_most:None
let whitespace = take_while ~f:is_whitespace ~at_least:1 ~at_most:None

let non_negative_integer =
  let%map digits =
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
  let%map minus = take_while ~f:(Char.( = ) '-') ~at_least:0 ~at_most:(Some 1)
  and value = non_negative_integer in
  match String.( = ) minus "" with
  | true -> value
  | false -> -value
;;
