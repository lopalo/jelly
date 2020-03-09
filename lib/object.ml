module S = String
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
  | Vec of t array
  | HashTable of (string, t) Hashtbl.t

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

and procedure =
  | Function of func
  | Closure of
      { lambda : lambda;
        scope : scope }
  | Apply
  | SyntaxExpander
  | CallWithCurrentContinuation
  | Continuation of stack

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

let procedure_to_string = function
  | Function _ -> "#function"
  | Closure _ -> "#closure"
  | Continuation _ -> "#continuation"
  | Apply
  | SyntaxExpander
  | CallWithCurrentContinuation ->
      "#procedure"

let rec to_string = function
  | Null -> "()"
  | Bool b -> string_of_bool b
  | Int i -> string_of_int i
  | Float f -> string_of_float f
  | Char c -> char_to_string c
  | Str s -> "\"" ^ S.escaped s ^ "\""
  | Sym (Symbol s, _) -> s
  | Cons (objs, _) -> "(" ^ (List.map to_string objs |> S.concat " ") ^ ")"
  | Procedure proc -> procedure_to_string proc
  | Vec arr ->
      let content = Array.map to_string arr |> Array.to_list |> S.concat " " in
      "#vec(" ^ content ^ ")"
  | HashTable htbl ->
      let content =
        Hashtbl.to_seq htbl |> Seq.map pair_to_string |> List.of_seq
        |> S.concat " "
      in
      "#hmap(" ^ content ^ ")"

and pair_to_string (k, v) = "[\"" ^ S.escaped k ^ "\" " ^ to_string v ^ "]"

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
  | Vec v, Vec v' -> v == v'
  | HashTable h, HashTable h' -> h == h'
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
    | Vec arr, Vec arr' -> equal_arrays arr arr'
    | HashTable h, HashTable h' -> equal_hashtables h h'
    | _, _ -> false

and equal_lists objs objs' =
  match (objs, objs') with
  | x :: xs, y :: ys -> equal x y && equal_lists xs ys
  | [], [] -> true
  | _, _ -> false

and equal_arrays arr arr' =
  let module A = Array in
  if A.length arr <> A.length arr' then false
  else
    let rec loop idx =
      if idx = A.length arr then true
      else if equal arr.(idx) arr'.(idx) then loop (succ idx)
      else false
    in
    loop 0

and equal_hashtables htbl htbl' =
  let module H = Hashtbl in
  if H.length htbl <> H.length htbl' then false
  else
    let rec loop = function
      | k :: keys -> (
        match (H.find_opt htbl k, H.find_opt htbl' k) with
        | Some v, Some v' -> if equal v v' then loop keys else false
        | _ -> false)
      | [] -> true
    in
    H.to_seq_keys htbl |> List.of_seq |> loop

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
  | Procedure _
  | Vec _
  | HashTable _ ->
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
  | Procedure proc -> Fmt.string ppf (procedure_to_string proc)
  | Vec arr ->
      Fmt.string ppf "#vec";
      Fmt.parens (Fmt.array ~sep:Fmt.sp (Fmt.box pp)) ppf arr
  | HashTable htbl ->
      Fmt.string ppf "#hmap";
      Fmt.parens (Fmt.hashtbl ~sep:Fmt.sp (Fmt.box pair_pp)) ppf htbl

and pair_pp ppf (k, v) =
  Fmt.string ppf "[";
  Fmt.box (Fmt.quote Fmt.string) ppf (S.escaped k);
  Fmt.sp ppf ();
  Fmt.box pp ppf v;
  Fmt.string ppf "]"

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
  | Procedure _
  | Vec _
  | HashTable _ ->
      None
