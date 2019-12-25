module S = String
module L = List
module Expr = Expression

let format = Printf.sprintf

(* TODO: Closure - a procedure created by interpreter *)
(*       Function - a wrapper over OCaml function *)
(* let procedure = Closure | Function  *)

type t =
  | Null
  | Bool of bool
  | Int of int
  | Float of float
  | Char of char
  | Str of string
  | Sym of Expr.Symbol.t * Common.meta option
  | Cons of t list * Common.meta option

(* TODO *)
(* | Procedure of procedure *)
(* | Vec of t array *)
(* | HashTable   *)
(* | Record   *)

let symbol ?meta name = Sym (Expr.Symbol.Symbol name, meta)

let list ?meta objs =
  match objs with
  | [] -> Null
  | _ -> Cons (objs, meta)

let char_to_string = function
  | '\n' -> "\\newline"
  | ' ' -> "\\space"
  | c ->
      let s = Char.escaped c in
      if S.length s = 1 then "\\" ^ s else s

let rec to_string = function
  | Null -> "()"
  | Bool b -> string_of_bool b
  | Int i -> string_of_int i
  | Float f -> string_of_float f
  | Char c -> char_to_string c
  | Str s -> "\"" ^ S.escaped s ^ "\""
  | Sym (Symbol s, _) -> s
  | Cons (objs, _) -> "(" ^ (L.map to_string objs |> S.concat " ") ^ ")"

(* TODO *)
(* | Vec _ -> "#vec()" *)
(* | HashTable -> "#hmap()" *)
(* | Record -> "#rec:record-name" *)

let rec equal obj obj' =
  if obj == obj' then true
  else
    match (obj, obj') with
    | Null, Null -> true
    | Bool b, Bool b' -> Bool.equal b b'
    | Int i, Int i' -> Int.equal i i'
    | Float f, Float f' -> Float.equal f f'
    | Char c, Char c' -> Char.equal c c'
    | Str s, Str s' -> S.equal s s'
    | Sym (Symbol s, _), Sym (Symbol s', _) -> S.equal s s'
    | Cons (objs, _), Cons (objs', _) -> equal_lists objs objs'
    | _ -> false

and equal_lists objs objs' =
  match (objs, objs') with
  | x :: xs, y :: ys -> equal x y && equal_lists xs ys
  | [], [] -> true
  | _ -> false

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

let rec pp ppf = function
  | Null -> Fmt.string ppf "()"
  | Bool b -> Fmt.bool ppf b
  | Int i -> Fmt.int ppf i
  | Float f -> Fmt.float ppf f
  | Char c -> Fmt.string ppf @@ char_to_string c
  | Str s -> Fmt.quote Fmt.string ppf @@ S.escaped s
  | Sym (Symbol s, _) -> Fmt.string ppf s
  | Cons (objs, _) -> Fmt.parens (Fmt.list ~sep:Fmt.sp (Fmt.box pp)) ppf objs
