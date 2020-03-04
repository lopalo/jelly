open Object

let bad_arg header obj = Error ("bad-arg." ^ header ^ ": " ^ to_string obj)

let bad_args header objs =
  Error
    ("bad-args." ^ header ^ ": " ^ String.concat ", "
   @@ List.map to_string objs)

let obj_to_string obj = Ok (Str (to_string obj))

let identical_object o o' = Ok (Bool (identical o o'))

let equal_object o o' = Ok (Bool (equal o o'))

let display = function
  | Str s -> print_string s; Ok Null
  | o -> bad_arg "display" o

let newline () = print_newline (); Ok Null

let readline () = Ok (Str (read_line ()))

let cons head = function
  | Null -> Ok (Cons ([head], None))
  | Cons (tail, _) -> Ok (Cons (head :: tail, None))
  | o -> bad_arg "cons.2" o

let car = function
  | Cons (head :: _, _) -> Ok head
  | o -> bad_arg "car" o

let cdr = function
  | Cons (_ :: tail, _) ->
      Ok
        (match tail with
        | [] -> Null
        | tail -> Cons (tail, None))
  | o -> bad_arg "cdr" o

let list objs = Ok (list ?meta:None objs)

let copy_meta = function
  | Sym (_, meta)
  | Cons (_, meta) -> (
      function
      | Sym (s, _) -> Ok (Sym (s, meta))
      | Cons (p, _) -> Ok (Cons (p, meta))
      | o -> bad_arg "copy-meta.2.must-support-meta" o)
  | o -> Fun.const @@ bad_arg "copy-meta.1.must-support-meta" o

let string_to_symbol = function
  | Str s -> Ok (Sym (Expression.symbol s, None))
  | o -> bad_arg "string->symbol" o

let is_symbol = function
  | Sym _ -> Ok (Bool true)
  | _ -> Ok (Bool false)

let is_integer = function
  | Int _ -> Ok (Bool true)
  | _ -> Ok (Bool false)

let plus o o' =
  match (o, o') with
  | Int i, Int i' -> Ok (Int (i + i'))
  | Float f, Float f' -> Ok (Float (f +. f'))
  | Int i, Float f -> Ok (Float (float_of_int i +. f))
  | Float f, Int i -> Ok (Float (f +. float_of_int i))
  | o, o' -> bad_args "+" [o; o']

let minus o o' =
  match (o, o') with
  | Int i, Int i' -> Ok (Int (i - i'))
  | Float f, Float f' -> Ok (Float (f -. f'))
  | Int i, Float f -> Ok (Float (float_of_int i -. f))
  | Float f, Int i -> Ok (Float (f -. float_of_int i))
  | o, o' -> bad_args "-" [o; o']

let multiplication o o' =
  match (o, o') with
  | Int i, Int i' -> Ok (Int (i * i'))
  | Float f, Float f' -> Ok (Float (f *. f'))
  | Int i, Float f -> Ok (Float (float_of_int i *. f))
  | Float f, Int i -> Ok (Float (f *. float_of_int i))
  | o, o' -> bad_args "*" [o; o']

let division o o' =
  match (o, o') with
  | Int i, Int i' -> Ok (Float (float_of_int i /. float_of_int i'))
  | Float f, Float f' -> Ok (Float (f /. f'))
  | Int i, Float f -> Ok (Float (float_of_int i /. f))
  | Float f, Int i -> Ok (Float (f /. float_of_int i))
  | o, o' -> bad_args "/" [o; o']

let string_to_integer = function
  | Str s -> Ok (Int (int_of_string s))
  | o -> bad_arg "string->integer" o

let string_to_float = function
  | Str s -> Ok (Int (int_of_string s))
  | o -> bad_arg "string->float" o

let truncate = function
  | Float f -> Ok (Int (truncate f))
  | Int _ as o -> Ok o
  | o -> bad_arg "truncate" o

let floor = function
  | Float f -> Ok (Float (floor f))
  | Int _ as o -> Ok o
  | o -> bad_arg "floor" o

let ceiling = function
  | Float f -> Ok (Float (ceil f))
  | Int _ as o -> Ok o
  | o -> bad_arg "ceiling" o

let round = function
  | Float f -> Ok (Float (Float.round f))
  | Int _ as o -> Ok o
  | o -> bad_arg "round" o

let equal_number o o' =
  match (o, o') with
  | Int i, Int i' -> Ok (Bool (Int.equal i i'))
  | Float f, Float f' -> Ok (Bool (Float.equal f f'))
  | Int i, Float f -> Ok (Bool (Float.equal (float_of_int i) f))
  | Float f, Int i -> Ok (Bool (Float.equal f (float_of_int i)))
  | o, o' -> bad_args "=" [o; o']

let greater_than o o' =
  match (o, o') with
  | Int i, Int i' -> Ok (Bool (i > i'))
  | Float f, Float f' -> Ok (Bool (f > f'))
  | Int i, Float f -> Ok (Bool (float_of_int i > f))
  | Float f, Int i -> Ok (Bool (f > float_of_int i))
  | o, o' -> bad_args ">" [o; o']

let greater_than_eq o o' =
  match (o, o') with
  | Int i, Int i' -> Ok (Bool (i >= i'))
  | Float f, Float f' -> Ok (Bool (f >= f'))
  | Int i, Float f -> Ok (Bool (float_of_int i >= f))
  | Float f, Int i -> Ok (Bool (f >= float_of_int i))
  | o, o' -> bad_args ">=" [o; o']

let less_than o o' =
  match (o, o') with
  | Int i, Int i' -> Ok (Bool (i < i'))
  | Float f, Float f' -> Ok (Bool (f < f'))
  | Int i, Float f -> Ok (Bool (float_of_int i < f))
  | Float f, Int i -> Ok (Bool (f < float_of_int i))
  | o, o' -> bad_args "<" [o; o']

let less_than_eq o o' =
  match (o, o') with
  | Int i, Int i' -> Ok (Bool (i <= i'))
  | Float f, Float f' -> Ok (Bool (f <= f'))
  | Int i, Float f -> Ok (Bool (float_of_int i <= f))
  | Float f, Int i -> Ok (Bool (f <= float_of_int i))
  | o, o' -> bad_args "<=" [o; o']

let not o = Ok (Bool (not (is_true o)))

let fail = function
  | Str error -> Error error
  | o -> bad_arg "fail" o

module Scope = Symbol.Map

let procedure f = Procedure (Function f)

(* Must not contain mutable objects *)
let objects =
  List.map
    (fun (name, obj) -> (Expression.symbol name, obj))
    [ ("object->string", procedure (Function1 obj_to_string));
      ("identical?", procedure (Function2 identical_object));
      ("equal?", procedure (Function2 equal_object));
      ("display", procedure (Function1 display));
      ("newline", procedure (Function0 newline));
      ("readline", procedure (Function0 readline));
      ("cons", procedure (Function2 cons));
      ("car", procedure (Function1 car));
      ("cdr", procedure (Function1 cdr));
      ("list", procedure (FunctionVariadic list));
      ("copy-meta", procedure (Function2 copy_meta));
      ("string->symbol", procedure (Function1 string_to_symbol));
      ("symbol?", procedure (Function1 is_symbol));
      ("integer?", procedure (Function1 is_integer));
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
      ("<=", procedure (Function2 less_than_eq));
      ("not", procedure (Function1 not));
      ("fail", procedure (Function1 fail));
      ("set-error-handler!", Null);
      ("reset-error-handler!", Null);
      ("apply", Procedure Apply);
      ("expand-syntax", Procedure SyntaxExpander);
      ("call/cc", Procedure CallWithCurrentContinuation);
      ("gensym", Null) ]

let definitions =
  List.fold_left
    (fun names (name, _) -> Symbol.Set.add name names)
    Symbol.Set.empty objects

let make_scope () =
  List.fold_left
    (fun scope (name, obj) -> Scope.add name (ref obj) scope)
    Scope.empty objects
