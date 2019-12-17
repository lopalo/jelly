module S = String
module L = List

let format = Printf.sprintf

type symbol = Symbol of string [@@unboxed]

type position =
  { line_number : int;
    column_number : int }

type meta = position

type t =
  | Null
  | Bool of bool
  | Int of int
  | Float of float
  | Char of char
  | Str of string
  | Sym of symbol
  | SymM of symbol * meta
  | Cons of t list
  | ConsM of t list * meta

(* | Vec of t array *)
(* | HashTable   *)
(* | Record   *)

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
  | Sym (Symbol s)
  | SymM (Symbol s, _) ->
      s
  | Cons objs
  | ConsM (objs, _) ->
      "(" ^ (L.map to_string objs |> S.concat " ") ^ ")"

(* | Vec _ -> "#vec()" *)
(* | HashTable -> "#hmap()" *)
(* | Record -> "#rec:record-name" *)

let rec is_equal obj obj' =
  if obj == obj' then true
  else
    match (obj, obj') with
    | Null, Null -> true
    | Bool b, Bool b' -> Bool.equal b b'
    | Int i, Int i' -> Int.equal i i'
    | Float f, Float f' -> Float.equal f f'
    | Char c, Char c' -> Char.equal c c'
    | Str s, Str s' -> S.equal s s'
    | Sym (Symbol s), Sym (Symbol s')
    | Sym (Symbol s), SymM (Symbol s', _)
    | SymM (Symbol s, _), Sym (Symbol s')
    | SymM (Symbol s, _), SymM (Symbol s', _) ->
        S.equal s s'
    | Cons objs, Cons objs'
    | Cons objs, ConsM (objs', _)
    | ConsM (objs, _), Cons objs'
    | ConsM (objs, _), ConsM (objs', _) ->
        is_eq_lists objs objs'
    | _ -> false

and is_eq_lists objs objs' =
  match (objs, objs') with
  | x :: xs, y :: ys -> is_equal x y && is_eq_lists xs ys
  | [], [] -> true
  | _ -> false

let bad_arg header obj = invalid_arg @@ header ^ ": " ^ to_string obj

let cons head = function
  | Null -> Cons [head]
  | Cons tail
  | ConsM (tail, _) ->
      Cons (head :: tail)
  | o -> bad_arg "cons:2" o

let car = function
  | Cons (head :: _)
  | ConsM (head :: _, _) ->
      head
  | o -> bad_arg "car" o

let cdr = function
  | Cons (_ :: tail)
  | ConsM (_ :: tail, _) -> (
    match tail with
    | [] -> Null
    | o :: _ -> o)
  | o -> bad_arg "cdr" o

let copy_meta = function
  | SymM (_, meta)
  | ConsM (_, meta) -> (
      function
      | Sym s
      | SymM (s, _) ->
          SymM (s, meta)
      | Cons p
      | ConsM (p, _) ->
          ConsM (p, meta)
      | o -> bad_arg "copy-meta:2" o)
  | o -> bad_arg "copy-meta:1:no-meta" o

let rec formatter ppf = function
  | Null -> Fmt.string ppf "()"
  | Bool b -> Fmt.bool ppf b
  | Int i -> Fmt.int ppf i
  | Float f -> Fmt.float ppf f
  | Char c -> Fmt.string ppf @@ char_to_string c
  | Str s -> Fmt.quote Fmt.string ppf @@ S.escaped s
  | Sym (Symbol s)
  | SymM (Symbol s, _) ->
      Fmt.string ppf s
  | Cons objs
  | ConsM (objs, _) ->
      Fmt.parens (Fmt.list ~sep:Fmt.sp (Fmt.box formatter)) ppf objs
