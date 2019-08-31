type 'a ctx = (string * 'a) list

let rec print ctx =
  match ctx with
  | (k, _) :: kvs ->
      print_string k ; print_string "," ; print kvs
  | [] ->
      print_endline ""

let empty () = []

let add k v ctx = (k, v) :: ctx

let lookup k n0 c =
  let rec loop kvs0 n0 =
    match (kvs0, n0) with
    | (k', _) :: kvs, n when k <> k' ->
        loop kvs n
    | (_, _) :: kvs, n when n > 0 ->
        loop kvs (n - 1)
    | (_, v) :: _, n when n = 0 ->
        Some v
    | _ ->
        None
  in
  loop c n0

let map f (ctx : 'a ctx) : 'a ctx = List.map f ctx
