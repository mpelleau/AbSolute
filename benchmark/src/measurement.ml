
type bench = {
  data_path: string;
  duration: Mtime.span
}

let csv_header = "path, time (ms)"

let bench_to_csv data =
  data.data_path ^ "," ^
  (string_of_float (Mtime.Span.to_ms data.duration))

let print_csv_line line =
  Printf.printf "%s\n" line;
  flush_all ()

let print_csv_header () = print_csv_line csv_header
let print_as_csv data = print_csv_line (bench_to_csv data)
