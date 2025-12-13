(* TODOOOO: have rg return all TODO/FIX/HACK *)
open Muaddept

(* TODO: This is a
   multi-line comment
   which I'd like to collect *)

let parse_rg_location (line : string) : int * Todo.location =
  try
    let i_null = String.index line '\000' in
    let i_line_end = String.index_from line (i_null + 1) ':' in
    let i_col_end = String.index_from line (i_line_end + 1) ':' in

    let result : Todo.location =
      {
        file = String.sub line 0 i_null;
        line =
          int_of_string (String.sub line (i_null + 1) (i_line_end - i_null - 1));
        column =
          int_of_string
            (String.sub line (i_line_end + 1) (i_col_end - i_line_end - 1));
      }
    in
    let read = i_col_end in
    (read, result)
  with
  | Not_found -> failwith ("Malformed output (missing delimiters): " ^ line)
  | Failure _ -> failwith ("Invalid integer in line/col: " ^ line)

let match_at (str : string) (pos : int) (prefix : string) : bool =
  let len_str = String.length str in
  let len_pre = String.length prefix in
  if len_str - pos < len_pre then false
  else
    let rec loop i =
      if i = len_pre then true
      else if str.[pos + i] <> prefix.[i] then false
      else loop (i + 1)
    in
    loop 0

let parse_rg_line_optimized (line : string) : Todo.t =
  let read, location = parse_rg_location line in

  let total_len = String.length line in
  let rec scan current_idx =
    if current_idx >= total_len then failwith ("No marker found in: " ^ line)
    else
      match
        List.find_opt
          (fun (marker_str, _) -> match_at line current_idx marker_str)
          Marker.all_strings
      with
      | None -> scan (current_idx + 1)
      | Some (_, marker) ->
          let msg = String.sub line current_idx (total_len - current_idx) in
          ({ location; marker; urgency = 0; msg } : Todo.t)
  in

  scan (read + 1)

let parse_rg_output (lines : string list) : Todo.t list =
  List.map parse_rg_line_optimized lines

let () =
  print_endline "Scanning for TODO/FIX/HACKs...";

  match Ripgrep.find_todos () |> Result.map parse_rg_output with
  | Error (Ripgrep.Execution_failed code) ->
      Printf.eprintf "Error: rg exited with code %d\n" code
  | Error (Ripgrep.Launch_failed msg) ->
      Printf.eprintf "Error: Failed to launch rg: %s\n" msg
  | Ok [] -> Printf.printf "✓ No issues found!\n"
  | Ok todos ->
      let oc = open_out_gen [ Open_append ] 0o644 "README.md" in
      let fmt = Format.formatter_of_out_channel oc in
      Dash_md.pp_dashbaord fmt todos;
      Format.pp_print_flush fmt ();
      close_out oc
