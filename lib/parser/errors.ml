open Lexing

(** iterate a function f from the lines n°[start] to n°[finish] of the file
    being processed *)
let iter_range file f start finish =
  let ic = open_in file in
  let rec loop cur =
    match In_channel.input_line ic with
    | Some l ->
        if cur > finish then close_in ic
        else if cur >= start then (
          f cur l ;
          loop (cur + 1) )
        else loop (cur + 1)
    | None -> close_in ic
  in
  loop 1

(** split the string [str] into two substring at index [idx] *)
let split_str_on (str : string) (idx : int) : string * string =
  let fsthalf = String.sub str 0 idx
  and secondhalf = String.sub str idx (String.length str - idx) in
  (fsthalf, secondhalf)

let show_error ?(padding = 5) file fmt ((ls, cs), (lf, cf)) =
  let show_line i l =
    if i = ls && i = lf then
      let fst, snd = split_str_on l cs in
      let snd, third = split_str_on snd (cf - cs) in
      Format.fprintf fmt "%i : %s\027[31m%s\027[0m%s\n" ls fst snd third
    else if i = ls then
      let fst, snd = split_str_on l cs in
      Format.fprintf fmt "%i : %s\027[31m%s\n" ls fst snd
    else if i = lf then
      let fst, snd = split_str_on l cf in
      Format.fprintf fmt "\027[0m%i :\027[31m %s\027[0m%s\n" i fst snd
    else if i > ls && i < lf then
      Format.fprintf fmt "\027[0m%i :\027[31m %s\n" i l
    else Format.fprintf fmt "%i : %s\n" i l
  in
  iter_range file show_line (max (ls - padding) 1) (lf + padding)

let pp_range file fmt ((ls, cs), (lf, cf)) =
  Format.fprintf fmt "file %s, Line %d, Column %d%s" file ls cs
    ( if ls = lf && cs = cf then ""
      else if ls = lf then Format.asprintf " to %d" cf
      else Format.asprintf " to Line %d, Column %d" lf cf )

let from_loc fmt (start, finish) =
  let col_s = start.pos_cnum - start.pos_bol in
  let col_f = finish.pos_cnum - finish.pos_bol in
  Format.fprintf fmt "%a\n%!" (pp_range start.pos_fname)
    ((start.pos_lnum, col_s), (finish.pos_lnum, col_f)) ;
  (show_error start.pos_fname)
    fmt
    ((start.pos_lnum, col_s), (finish.pos_lnum, col_f)) ;
  Format.fprintf fmt "%!"

let from_lex fmt lex =
  let start = lex.lex_start_p in
  let finish = lex.lex_curr_p in
  from_loc fmt (start, finish)
