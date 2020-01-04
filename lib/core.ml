open Object

(* TODO: all the functions must return (Object.t, string) result *)
let bad_arg header obj = invalid_arg @@ header ^ ": " ^ to_string obj

let bad_args header objs =
  invalid_arg @@ header ^ ": " ^ String.concat ", " @@ List.map to_string objs

let obj_to_string obj = Str (to_string obj)

let equal_object o o' = Bool (equal o o')

let display = function
  | Str s -> print_string s; Null
  | o -> bad_arg "display" o

let newline () = print_newline (); Null

let readline () = Str (read_line ())

let cons head = function
  | Null -> Cons ([head], None)
  | Cons (tail, _) -> Cons (head :: tail, None)
  | o -> bad_arg "cons.2" o

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
      | o -> bad_arg "copy-meta.2.cannot-have-meta" o)
  | o -> bad_arg "copy-meta.1.no-meta" o

let string_to_symbol = function
  | Str s -> Sym (Expression.Symbol.Symbol s, None)
  | o -> bad_arg "string->symbol" o

let is_symbol = function
  | Sym _ -> Bool true
  | _ -> Bool false

let plus o o' =
  match (o, o') with
  | Int i, Int i' -> Int (i + i')
  | Float f, Float f' -> Float (f +. f')
  | Int i, Float f -> Float (float_of_int i +. f)
  | Float f, Int i -> Float (f +. float_of_int i)
  | o, o' -> bad_args "+" [o; o']

let minus o o' =
  match (o, o') with
  | Int i, Int i' -> Int (i - i')
  | Float f, Float f' -> Float (f -. f')
  | Int i, Float f -> Float (float_of_int i -. f)
  | Float f, Int i -> Float (f -. float_of_int i)
  | o, o' -> bad_args "-" [o; o']

let multiplication o o' =
  match (o, o') with
  | Int i, Int i' -> Int (i * i')
  | Float f, Float f' -> Float (f *. f')
  | Int i, Float f -> Float (float_of_int i *. f)
  | Float f, Int i -> Float (f *. float_of_int i)
  | o, o' -> bad_args "*" [o; o']

let division o o' =
  match (o, o') with
  | Int i, Int i' -> Float (float_of_int i /. float_of_int i')
  | Float f, Float f' -> Float (f /. f')
  | Int i, Float f -> Float (float_of_int i /. f)
  | Float f, Int i -> Float (f /. float_of_int i)
  | o, o' -> bad_args "/" [o; o']

let string_to_integer = function
  | Str s -> Int (int_of_string s)
  | o -> bad_arg "string->integer" o

let string_to_float = function
  | Str s -> Int (int_of_string s)
  | o -> bad_arg "string->float" o

let truncate = function
  | Float f -> Int (truncate f)
  | o -> bad_arg "truncate" o

let floor = function
  | Float f -> Float (floor f)
  | o -> bad_arg "floor" o

let ceiling = function
  | Float f -> Float (ceil f)
  | o -> bad_arg "ceiling" o

let round = function
  | Float f -> Float (Float.round f)
  | o -> bad_arg "round" o

let equal_number o o' =
  Bool
    (match (o, o') with
    | Int i, Int i' -> Int.equal i i'
    | Float f, Float f' -> Float.equal f f'
    | Int i, Float f -> Float.equal (float_of_int i) f
    | Float f, Int i -> Float.equal f (float_of_int i)
    | o, o' -> bad_args "=" [o; o'])

let greater_than o o' =
  Bool
    (match (o, o') with
    | Int i, Int i' -> i > i'
    | Float f, Float f' -> f > f'
    | Int i, Float f -> float_of_int i > f
    | Float f, Int i -> f > float_of_int i
    | o, o' -> bad_args ">" [o; o'])

let greater_than_eq o o' =
  Bool
    (match (o, o') with
    | Int i, Int i' -> i >= i'
    | Float f, Float f' -> f >= f'
    | Int i, Float f -> float_of_int i >= f
    | Float f, Int i -> f >= float_of_int i
    | o, o' -> bad_args ">=" [o; o'])

let less_than o o' =
  Bool
    (match (o, o') with
    | Int i, Int i' -> i < i'
    | Float f, Float f' -> f < f'
    | Int i, Float f -> float_of_int i < f
    | Float f, Int i -> f < float_of_int i
    | o, o' -> bad_args "<" [o; o'])

let less_than_eq o o' =
  Bool
    (match (o, o') with
    | Int i, Int i' -> i <= i'
    | Float f, Float f' -> f <= f'
    | Int i, Float f -> float_of_int i <= f
    | Float f, Int i -> f <= float_of_int i
    | o, o' -> bad_args "<=" [o; o'])

module Scope = Expression.SymbolMap
module SymbolSet = Expression.SymbolSet

let procedure f = Procedure (Function f)

(* Must not contain mutable objects *)
let objects =
  List.map
    (fun (name, obj) -> (Expression.Symbol.Symbol name, obj))
    [ ("->string", procedure (Function1 obj_to_string));
      ("equal?", procedure (Function2 equal_object));
      ("display", procedure (Function1 display));
      ("newline", procedure (Function0 newline));
      ("readline", procedure (Function0 readline));
      ("cons", procedure (Function2 cons));
      ("car", procedure (Function1 car));
      ("cdr", procedure (Function1 cdr));
      ("list", procedure (FunctionVariadic (list ?meta:None)));
      ("copy-meta", procedure (Function2 copy_meta));
      ("string->symbol", procedure (Function1 string_to_symbol));
      ("symbol?", procedure (Function1 is_symbol));
      ("+", procedure (Function2 plus));
      ("-", procedure (Function2 minus));
      ("*", procedure (Function2 multiplication));
      ("/", procedure (Function2 division));
      ("string->integer", procedure (Function1 string_to_integer));
      ("string->float", procedure (Function1 string_to_float));
      ("truncate", procedure (Function1 truncate));
      ("floor", procedure (Function1 floor));
      ("ceiling", procedure (Function1 ceiling));
      ("round", procedure (Function1 round));
      ("=", procedure (Function2 equal_number));
      (">", procedure (Function2 greater_than));
      (">=", procedure (Function2 greater_than_eq));
      ("<", procedure (Function2 less_than));
      ("<=", procedure (Function2 less_than_eq)) ]

let definitions =
  List.fold_left
    (fun names (name, _) -> SymbolSet.add name names)
    SymbolSet.empty objects

let make_scope () =
  List.fold_left
    (fun scope (name, obj) -> Scope.add name (ref obj) scope)
    Scope.empty objects
