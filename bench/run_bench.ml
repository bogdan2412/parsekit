open! Core
open! Import

let make_bench ~name ~path kind_ =
  let contents = Stdio.In_channel.read_all path in
  let parser =
    match kind_ with
    | `Json -> Parsekit.ignore_m Json_parsekit.parser
    | `Utf8 -> Parsekit.skip_strict_utf8
  in
  (* Do a single run at top-level to ensure parsing does not raise. *)
  Parsekit.run ~require_input_entirely_consumed:true parser contents;
  Bench.Test.create ~name (fun () ->
    Parsekit.run ~require_input_entirely_consumed:true parser contents)
;;

let main () =
  let nativejson_bench_suite =
    let dir = "data/nativejson-benchmark" in
    Array.to_list (Stdlib.Sys.readdir dir)
    |> List.filter ~f:(fun name ->
      String.( <> ) name "LICENSE" && String.( <> ) name "ACKNOWLEDGEMENTS")
    |> List.sort ~compare:String.compare
    |> List.map ~f:(fun name -> name, Stdlib.Filename.concat dir name)
    |> List.map ~f:(fun (name, path) ->
      let name = [%string "Json: %{name}"] in
      make_bench ~name ~path `Json)
  in
  let custom_bench_suite =
    let dir = "data/custom" in
    Array.to_list (Stdlib.Sys.readdir dir)
    |> List.sort ~compare:String.compare
    |> List.map ~f:(fun name -> name, Stdlib.Filename.concat dir name)
  in
  let custom_bench_suite =
    List.concat_map [ `Json; `Utf8 ] ~f:(fun kind_ ->
      List.map custom_bench_suite ~f:(fun (name, path) ->
        let kind_sexp = [%sexp (kind_ : [ `Json | `Utf8 ])] in
        let name = [%string "%{kind_sexp#Sexp}: %{name}"] in
        make_bench ~name ~path kind_))
  in
  let bench_command = Bench.make_command (nativejson_bench_suite @ custom_bench_suite) in
  Command_unix.run bench_command
;;

let () = main ()
