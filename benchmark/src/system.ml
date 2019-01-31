open Printf

let json_ext = ".json"
let absolute_ext = ".abs"
let psplib_ext = ".sm"
let patterson_ext = ".rcp"
let usage = "Usage: absolute_bench <configuration file>\n"

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
