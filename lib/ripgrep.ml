type process_error =
  | Launch_failed of string
  (* | Read_failed of string *)
  | Execution_failed of int

let rg_command =
  [|
    "rg";
    "--null";
    "--no-heading";
    "--line-number";
    "--column";
    "--glob=!README.md";
    "TODO.*:|FIXME.*:|HACK.*:";
  |]

let run_rg (args : string array) : (string list, process_error) result =
  let cmd = args.(0) in
  match Unix.open_process_args_in cmd args with
  | exception Unix.Unix_error (err, _, _) ->
      Error (Launch_failed (Unix.error_message err))
  | channel -> (
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

let find_todos () : (string list, process_error) result = run_rg rg_command
