module S = String
module L = List

let format = Printf.sprintf

type symbol = Symbol of string [@@unboxed]

type position =
  { line_number : int;
    column_number : int }

type meta = position

(* TODO: HashTable, Record *)
type t =
  | Null
  | Bool of bool
  | Int of int
  | Float of float
  | Char of char
  | Str of string
  | Sym of symbol
  | Cons of t list
  | Vec of t array
  | SymM of symbol * meta
  | ConsM of t list * meta

let rec to_string = function
  | Null -> "()"
  | Bool b -> string_of_bool b
  | Int i -> string_of_int i
  | Float f -> string_of_float f
  | Char c -> (
    match c with
    | '\n' -> "\\newline"
    | ' ' -> "\\space"
    | _ -> "\\" ^ Char.escaped c)
  | Str s -> "\"" ^ s ^ "\""
  | Sym (Symbol s)
  | SymM (Symbol s, _) ->
      s
  | Cons objs
  | ConsM (objs, _) ->
      "(" ^ (L.map to_string objs |> S.concat " ") ^ ")"
  | Vec _ -> "#vec()"

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
