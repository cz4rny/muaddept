type t = Todo | Fix | Bug | Hack [@@deriving enumerate]

let info = function
  | Todo -> ("TODO", [])
  | Fix -> ("FIX", [ "FIXME" ])
  | Bug -> ("BUG", [])
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
