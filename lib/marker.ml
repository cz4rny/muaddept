type t = Todo | Fix | Hack [@@deriving enumerate]

let info = function
  | Todo -> ("TODO", [])
  | Fix -> ("FIX", [ "FIXME" ])
  | Hack -> ("HACK", [])

(* Compile-time check: exhaustiveness *)
let () = List.iter (fun m -> ignore (info m)) all
let to_string m = fst (info m)

let all_strings =
  List.concat_map
    (fun m ->
      let canonical, aliases = info m in
      canonical :: aliases |> List.map (fun s -> (s, m)))
    all
