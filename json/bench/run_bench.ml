open! Core
open! Import

let make_bench ~name ~path =
  let contents = Stdio.In_channel.read_all path in
  Bench.Test.create ~name (fun () ->
    Parsekit.run ~require_input_entirely_consumed:true Json_parsekit.parser contents)
;;

let main () =
  let nativejson_bench_suite =
    let dir = "data/nativejson-benchmark" in
    Array.to_list (Stdlib.Sys.readdir dir)
    |> List.filter ~f:(fun name ->
      String.( <> ) name "LICENSE" && String.( <> ) name "ACKNOWLEDGEMENTS")
    |> List.sort ~compare:String.compare
    |> List.map ~f:(fun name -> name, Stdlib.Filename.concat dir name)
    |> List.map ~f:(fun (name, path) -> make_bench ~name ~path)
  in
  let custom_bench_suite =
    let dir = "data/custom" in
    Array.to_list (Stdlib.Sys.readdir dir)
    |> List.sort ~compare:String.compare
    |> List.map ~f:(fun name -> name, Stdlib.Filename.concat dir name)
    |> List.map ~f:(fun (name, path) -> make_bench ~name ~path)
  in
  let bench_command = Bench.make_command (nativejson_bench_suite @ custom_bench_suite) in
  Command_unix.run bench_command
;;

let () = main ()
