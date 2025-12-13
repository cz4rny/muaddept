type process_error =
  | Launch_failed of string
  (* | Read_failed of string *)
  | Execution_failed of int

val find_todos : bool -> (string list, process_error) result
