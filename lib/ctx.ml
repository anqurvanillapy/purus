type 'a ctx = (string * 'a) list

let empty () = []

let add k v ctx = (k, v) :: ctx

let lookup k n0 c =
  let rec loop kvs0 n0 =
    match (kvs0, n0) with
    | (k', _) :: kvs, n when k != k' ->
        loop kvs n
    | (_, _) :: kvs, n when n > 0 ->
        loop kvs (n - 1)
    | (_, v) :: _, n when n == 0 ->
        Some v
    | (_, _) :: _, _ ->
        None
    | [], _ ->
        None
  in
  loop c n0

let rec map f (c : 'a ctx) : 'a ctx =
  match c with x :: xs -> f x :: map f xs | [] -> empty ()
