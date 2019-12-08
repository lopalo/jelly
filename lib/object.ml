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
  | Pair of pair
  | Vec of t array
  | SymM of symbol * meta
  | PairM of pair * meta

and pair =
  { mutable car : t;
    mutable cdr : t }

let cons car cdr = {car; cdr}

let is_proper_pair = function
  | Null -> true
  | Pair {cdr; _}
  | PairM ({cdr; _}, _) -> (
    match cdr with
    | Null
    | Pair _
    | PairM _ ->
        true
    | _ -> false)
  | _ -> false

let pairs_of_list objects =
  List.fold_right (fun head tail -> Pair (cons head tail)) objects Null

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
  | (Pair _ as p)
  | (PairM _ as p) ->
      "(" ^ pair_to_string p ^ ")"
  | Vec _ -> "(#vector)"

and pair_to_string = function
  | Pair {car; cdr = Null}
  | PairM ({car; cdr = Null}, _) ->
      to_string car
  | (Pair {car; cdr} as p)
  | (PairM ({car; cdr}, _) as p) ->
      if is_proper_pair p then to_string car ^ " " ^ pair_to_string cdr
      else format "#pair %s %s" (to_string car) (to_string cdr)
  | o -> to_string o

let bad_arg func_name obj = invalid_arg @@ func_name ^ ": " ^ to_string obj

let set_meta obj meta =
  match obj with
  | Sym s
  | SymM (s, _) ->
      SymM (s, meta)
  | Pair p
  | PairM (p, _) ->
      PairM (p, meta)
  | o -> bad_arg "set_meta" o
