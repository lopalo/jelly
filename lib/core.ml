open Object

(* TODO: all the functions must return (Object.t, string) result *)
let bad_arg header obj = invalid_arg @@ header ^ ": " ^ to_string obj

let is_symbol = function
  | Sym _ -> true
  | _ -> false

let cons head = function
  | Null -> Cons ([head], None)
  | Cons (tail, _) -> Cons (head :: tail, None)
  | o -> bad_arg "cons:2" o

let car = function
  | Cons (head :: _, _) -> head
  | o -> bad_arg "car" o

let cdr = function
  | Cons (_ :: tail, _) -> (
    match tail with
    | [] -> Null
    | o :: _ -> o)
  | o -> bad_arg "cdr" o

let copy_meta = function
  | Sym (_, meta)
  | Cons (_, meta) -> (
      function
      | Sym (s, _) -> Sym (s, meta)
      | Cons (p, _) -> Cons (p, meta)
      | o -> bad_arg "copy-meta:2:cannot-have-meta" o)
  | o -> bad_arg "copy-meta:1:no-meta" o
