open Muaddept

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

let match_at (str : string) (pos : int) (prefix : string) : (int * int) option =
  let len_str = String.length str in
  let len_pre = String.length prefix in
  if len_str - pos < len_pre then None
  else
    let urgency_char = prefix.[len_pre - 1] in

    let rec loop (i : int) (urgency : int) =
      let current_abs_pos = pos + i in

      if current_abs_pos >= len_str then
        if i >= len_pre then Some (pos + i, urgency) else None
      else if i >= len_pre then
        if str.[pos + i] <> urgency_char then Some (pos + i, urgency)
        else loop (i + 1) (urgency + 1)
      else if str.[pos + i] <> prefix.[i] then None
      else loop (i + 1) 0
    in
    loop 0 0

let parse_rg_line (line : string) : Todo.t option =
  let read, location = parse_rg_location line in

  let total_len = String.length line in
  let rec scan current_idx : Todo.t option =
    if current_idx >= total_len then (
      Printf.printf "No marker found in: %s\n" line;
      None)
    else
      let try_match (marker_str, marker) =
        match match_at line current_idx marker_str with
        | None -> None
        | Some (idx_read, urgency) -> Some (idx_read, marker, urgency)
      in
      match List.find_map try_match Marker.all_strings with
      | None -> scan (current_idx + 1)
      | Some (idx_read, marker, urgency) -> (
          match String.index_from_opt line idx_read ' ' with
          | None ->
              Printf.printf "No message on line: %s" line;
              None
          | Some idx_space ->
              let msg =
                String.trim (String.sub line idx_space (total_len - idx_space))
              in
              Some ({ location; marker; urgency; msg } : Todo.t))
  in

  scan (read + 1)

let parse_rg_output (lines : string list) : Todo.t list =
  List.filter_map parse_rg_line lines

let () =
  let include_readme = ref false in
  let specs =
    [
      ( "--include-readme",
        Arg.Set include_readme,
        "Include README.md in the scan" );
    ]
  in
  Arg.parse specs (fun _ -> ()) "Usage: muaddept [options]";

  print_endline "Scanning for TODOs, FIXes, FIXMEs, BUGs, and HACKs...";

  match Ripgrep.find_todos !include_readme |> Result.map parse_rg_output with
  | Error (Ripgrep.Execution_failed code) ->
      Printf.eprintf "Error: rg exited with code %d\n" code
  | Error (Ripgrep.Launch_failed msg) ->
      Printf.eprintf "Error: Failed to launch rg: %s\n" msg
  | Ok [] -> Printf.eprintf "✓ No issues found!\n"
  | Ok todos ->
      let oc = open_out_gen [ Open_append ] 0o644 "README.md" in
      let fmt = Format.formatter_of_out_channel oc in
      Dash_md.pp_dashbaord fmt todos;
      Format.pp_print_flush fmt ();
      close_out oc
