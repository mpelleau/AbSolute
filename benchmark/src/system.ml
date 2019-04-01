open Printf
open Bench_desc_t

let json_ext = ".json"
let absolute_ext = ".abs"
let psplib_ext = ".sm"
let patterson_ext = ".rcp"
let pro_gen_ext = ".sch"
let usage = "Usage: absolute_bench <configuration file>\n"

let extension_of_problem_kind = function
  | `PSPlib -> psplib_ext
  | `Patterson -> patterson_ext
  | `ProGenMax -> pro_gen_ext

let print_warning msg =
  eprintf "[Warning] %s\n%!" msg

let eprintf_and_exit msg =
  eprintf "[Error] %s\n%!" msg;
  exit 1

let file_argument fname =
  if Filename.check_suffix fname json_ext
  then
    fname
  else
    eprintf_and_exit (sprintf
      "Unknown file extension: %s. We expect `%s`.\n" fname json_ext)

let find_file fname =
  if Sys.file_exists fname then
    fname
  else begin
    Printf.printf "%s" usage;
    eprintf_and_exit (sprintf
      "Cannot find the file %s\n%!" fname)
  end

let file_to_string fname =
  let fpath = find_file fname in
  begin
    let ic = open_in fpath in
    try
      let content = Std.input_all ic in
      close_in ic;
      content
    with e ->
      close_in ic;
      eprintf_and_exit (sprintf
        "Reading file %s failed.\nError: %s." fname (Printexc.to_string e))
  end

let get_bench_desc () =
  if Array.length Sys.argv <> 2 then
  begin
    eprintf_and_exit "Benchmarks description file missing (see benchmark/data/example.json)."
  end
  else file_to_string (Array.get Sys.argv 1)

let check_problem_file_format config problem_path =
  let ext = extension_of_problem_kind config.problem_kind in
  if Sys.is_directory problem_path then begin
    print_warning ("subdirectory " ^ problem_path ^ " ignored.");
    false end
  else if (String.lowercase_ascii (Filename.extension problem_path)) <> ext then begin
    print_warning ("file \"" ^ problem_path ^
      "\" ignored (expected extension `" ^ ext ^ "`).");
    false end
  else
    true

let list_of_problems config =
  if Sys.is_directory config.problem_set then
    let files = Sys.readdir config.problem_set in
    Array.sort compare files;
    Array.to_list files |>
    List.map (fun x -> config.problem_set ^ x) |>
    List.filter (check_problem_file_format config)
  else
    if check_problem_file_format config config.problem_set then
      [config.problem_set]
    else
      []

let call_command command =
  flush_all ();
  let status = Sys.command command in
  status

let time_of coeff time =
  Mtime.Span.of_uint64_ns (Int64.mul (Int64.of_int coeff) (Int64.of_int time))

let time_of_ms = time_of 1000000
let time_of_sec = time_of 1000000000

let timeout_of_config config = time_of_sec config.timeout
