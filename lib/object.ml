module S = String
module L = List
module Expr = Expression

let format = Printf.sprintf

type t =
  | Null
  | Bool of bool
  | Int of int
  | Float of float
  | Char of char
  | Str of string
  | Sym of Symbol.t * Common.meta option
  | Cons of t list * Common.meta option
  | Procedure of procedure

and expression = t Expr.t

and lambda = t Expr.lambda

and variable = t ref

and scope = variable Symbol.Map.t

and stack_frame =
  { expression : expression;
    scope : scope }

and stack = stack_frame list

and func_res = (t, string) result

and func =
  | FunctionVariadic of (t list -> func_res)
  | Function0 of (unit -> func_res)
  | Function1 of (t -> func_res)
  | Function2 of (t -> t -> func_res)
  | Function3 of (t -> t -> t -> func_res)

(* TODO *)
(* | Vec of t array *)
(* | HashTable   *)
and procedure =
  | Function of func
  | Closure of
      { lambda : lambda;
        scope : scope }
  | Apply
  | SyntaxExpander

(* TODO *)
(* | Continuation of stack *)

let symbol ?meta name = Sym (Expr.symbol name, meta)

let list ?meta = function
  | [] -> Null
  | objs -> Cons (objs, meta)

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
  | Procedure _ -> "#procedure"

(* TODO *)
(* | Vec -> "#vec(...)" *)
(* | HashTable -> "#hmap(...)" *)

let identical obj obj' =
  match (obj, obj') with
  | Null, Null -> true
  | Bool b, Bool b' -> b == b'
  | Int i, Int i' -> i == i'
  | Float f, Float f' -> f == f'
  | Char c, Char c' -> c == c'
  | Str s, Str s' -> s == s'
  | Sym (s, m), Sym (s', m') -> s == s' && m == m'
  | Cons (objs, m), Cons (objs', m') -> objs == objs' && m == m'
  | obj, obj' -> obj == obj'

let rec equal obj obj' =
  if identical obj obj' then true
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
    | _, _ -> false

and equal_lists objs objs' =
  match (objs, objs') with
  | x :: xs, y :: ys -> equal x y && equal_lists xs ys
  | [], [] -> true
  | _, _ -> false

let is_true = function
  | Null
  | Bool false ->
      false
  | Bool true -> true
  | Int _
  | Float _
  | Char _
  | Str _
  | Sym _
  | Cons _
  | Procedure _ ->
      true

let rec pp ppf = function
  | Null -> Fmt.string ppf "()"
  | Bool b -> Fmt.bool ppf b
  | Int i -> Fmt.int ppf i
  | Float f -> Fmt.float ppf f
  | Char c -> Fmt.string ppf @@ char_to_string c
  | Str s -> Fmt.quote Fmt.string ppf @@ S.escaped s
  | Sym (Symbol s, _) -> Fmt.string ppf s
  | Cons (objs, _) -> Fmt.parens (Fmt.list ~sep:Fmt.sp (Fmt.box pp)) ppf objs
  | Procedure _ -> Fmt.string ppf "#procedure"

let meta = function
  | Sym (_, meta)
  | Cons (_, meta) ->
      meta
  | Null
  | Bool _
  | Int _
  | Float _
  | Char _
  | Str _
  | Procedure _ ->
      None
