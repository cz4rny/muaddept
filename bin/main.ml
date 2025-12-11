(* TODOOOO: have rg return all TODO/FIX/HACK *)

(* TODO: This is a
   multi-line comment
   which I'd like to collect *)

let or_fail msg = function Some x -> x | None -> failwith msg

let rg_command =
  [|
    "rg";
    "--no-heading";
    "--line-number";
    "--column";
    "--glob=!README.md";
    "TODO*:|FIXME.*:|HACK.*:";
  |]

type process_error =
  | Launch_failed of string
  (* | Read_failed of string *)
  | Execution_failed of int

let run_command (args : string array) : (string list, process_error) result =
  let cmd = args.(0) in
  match Unix.open_process_args_in cmd args with
  | exception Unix.Unix_error (err, _, _) ->
      Error (Launch_failed (Unix.error_message err))
  | channel -> (
      (* Now handle reading with guaranteed cleanup *)
      match In_channel.input_lines channel with
      | exception exn ->
          ignore (Unix.close_process_in channel);
          raise exn
      | output -> (
          match Unix.close_process_in channel with
          | Unix.WEXITED 0 -> Ok output
          | Unix.WEXITED 1 -> Ok []
          | Unix.WEXITED code -> Error (Execution_failed code)
          | _ -> Error (Execution_failed (-1))))

module Marker = struct
  type t = Todo | Fix | Hack [@@deriving enumerate]

  (* Single source of truth *)
  let info = function
    | Todo -> ("TODO", [])
    | Fix -> ("FIX", [ "FIXME" ])
    | Hack -> ("HACK", [])

  let to_string m = fst (info m)

  (* let all_strings = *)
  (*   List.concat_map *)
  (*     (fun m -> *)
  (*       let canonical, aliases = info m in *)
  (*       canonical :: aliases |> List.map (fun s -> (s, m))) *)
  (*     all *)

  (* let of_string s = List.assoc_opt s all_strings *)

  (* Compile-time check: exhaustiveness *)
  (* let () = List.iter (fun m -> ignore (info m)) all *)
end

type todo_item = {
  kind : Marker.t;
  file : string;
  line : int;
  column : int;
  urgency : int;
  msg : string;
}

let parse_rg_line (line : string) : todo_item =
  let parse_metadata (line : string) =
    Scanf.sscanf_opt line "%[^:]:%d:%d:%n" (fun f l c o -> (f, l, c, o))
    |> or_fail ("Error extracting metadata from: " ^ line)
  in
  let file, line_num, column, offset = parse_metadata line in
  let content = String.sub line offset (String.length line - offset) in
  let len = String.length content in
  let rec scan (i : int) : Marker.t * string =
    if i >= len then failwith ("No marker found in " ^ content)
    else
      let window = String.sub content i (len - i) in
      match
        List.find_map
          (fun (m : Marker.t) ->
            if not (String.starts_with ~prefix:(Marker.to_string m) window) then
              None
            else Some (m, window))
          Marker.all
      with
      | None -> scan (i + 1)
      | Some r -> r
  in
  let kind, msg = scan 0 in
  { file; line = line_num; column; kind; urgency = 0; msg }

let parse_rg_output (lines : string list) : todo_item list =
  List.map parse_rg_line lines

let () =
  print_endline "Scanning for TODO/FIX/HACKs...";

  match run_command rg_command |> Result.map parse_rg_output with
  | Error (Execution_failed code) ->
      Printf.eprintf "Error: rg exited with code %d\n" code
  | Error (Launch_failed msg) ->
      Printf.eprintf "Error: Failed to launch rg: %s\n" msg
  | Ok [] -> Printf.printf "✓ No issues found!\n"
  | Ok todos ->
      Printf.printf "Found %d todos:\n" (List.length todos);
      List.iter
        (fun t ->
          Printf.printf "%s(%d): %s [%s:%d:%d]\n" (Marker.to_string t.kind)
            t.urgency t.msg t.file t.line t.column)
        todos
