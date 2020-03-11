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

let is_list = function
  | Null
  | Cons _ ->
      Ok (Bool true)
  | _ -> Ok (Bool false)

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

let is_bool = function
  | Bool _ -> Ok (Bool true)
  | _ -> Ok (Bool false)

let is_integer = function
  | Int _ -> Ok (Bool true)
  | _ -> Ok (Bool false)

let is_float = function
  | Float _ -> Ok (Bool true)
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

let is_char = function
  | Char _ -> Ok (Bool true)
  | _ -> Ok (Bool false)

let is_string = function
  | Str _ -> Ok (Bool true)
  | _ -> Ok (Bool false)

let string_length = function
  | Str s -> Ok (Int (String.length s))
  | o -> bad_arg "string-length" o

let substring string start length =
  match (string, start, length) with
  | Str s, Int pos, Int len -> (
    try Ok (Str (String.sub s pos len))
    with Invalid_argument _ ->
      bad_args "substring.invalid-positions" [start; length])
  | string, start, length -> bad_args "substring" [string; start; length]

let string_to_list = function
  | Str s ->
      Ok
        (String.to_seq s
        |> Seq.map (fun c -> Char c)
        |> List.of_seq |> Object.list)
  | o -> bad_arg "string->list" o

let list_to_string = function
  | Cons (chars, _)
    when List.for_all
           (function
             | Char _ -> true
             | _ -> false)
           chars ->
      Ok
        (Str
           (List.map (fun [@warning "-8"] (Char c) -> c) chars
           |> Parser.string_of_chars))
  | Null -> Ok (Str "")
  | o -> bad_arg "list->string" o

let string_append o o' =
  match (o, o') with
  | Str s, Str s' -> Ok (Str (s ^ s'))
  | o, o' -> bad_args "string-append" [o; o']

let vector objs = Ok (Vec (Array.of_list objs))

let make_vector n obj =
  match n with
  | Int n when n >= 0 -> Ok (Vec (Array.make n obj))
  | o -> bad_arg "make-vector" o

let is_vector = function
  | Vec _ -> Ok (Bool true)
  | _ -> Ok (Bool false)

let vector_length = function
  | Vec arr -> Ok (Int (Array.length arr))
  | o -> bad_arg "vector-length" o

let vector_ref vec n =
  match (vec, n) with
  | Vec arr, Int n ->
      if n >= 0 && n < Array.length arr then Ok arr.(n)
      else bad_arg "vector-ref.2.out-of-bounds" (Int n)
  | vec, n -> bad_args "vector-ref" [vec; n]

let vector_set vec n obj =
  match (vec, n) with
  | Vec arr, Int n ->
      if n >= 0 && n < Array.length arr then (arr.(n) <- obj; Ok Null)
      else bad_arg "vector-set!.2.out-of-bounds" (Int n)
  | vec, n -> bad_args "vector-set!" [vec; n]

let list_to_vector = function
  | Cons (objs, _) -> Ok (Vec (Array.of_list objs))
  | Null -> Ok (Vec (Array.of_list []))
  | o -> bad_arg "list->vector" o

let vector_to_list = function
  | Vec arr -> Ok (Array.to_list arr |> Object.list ?meta:None)
  | o -> bad_arg "vector->list" o

let make_hashtable = function
  | Int size -> Ok (HashTable (Hashtbl.create size))
  | o -> bad_arg "make-hashtable" o

let is_hashtable = function
  | HashTable _ -> Ok (Bool true)
  | _ -> Ok (Bool false)

let hashtable_size = function
  | HashTable h -> Ok (Int (Hashtbl.length h))
  | htbl -> bad_arg "hashtable-size" htbl

let hashtable_keys = function
  | HashTable h ->
      Ok
        (Vec (Hashtbl.to_seq_keys h |> Seq.map (fun k -> Str k) |> Array.of_seq))
  | htbl -> bad_arg "hashtable-keys" htbl

let hashtable_ref htbl key default =
  match (htbl, key) with
  | HashTable h, Str k -> (
    match Hashtbl.find_opt h k with
    | Some v -> Ok v
    | None -> Ok default)
  | htbl, key -> bad_args "hashtable-ref" [htbl; key]

let hashtable_set htbl key value =
  match (htbl, key) with
  | HashTable h, Str k -> Hashtbl.replace h k value; Ok Null
  | htbl, key -> bad_args "hashtable-set!" [htbl; key]

let hashtable_delete htbl key =
  match (htbl, key) with
  | HashTable h, Str k -> Hashtbl.remove h k; Ok Null
  | htbl, key -> bad_args "hashtable-delete!" [htbl; key]

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
      ("list?", procedure (Function1 is_list));
      ("cons", procedure (Function2 cons));
      ("car", procedure (Function1 car));
      ("cdr", procedure (Function1 cdr));
      ("list", procedure (FunctionVariadic list));
      ("copy-meta", procedure (Function2 copy_meta));
      ("string->symbol", procedure (Function1 string_to_symbol));
      ("symbol?", procedure (Function1 is_symbol));
      ("bool?", procedure (Function1 is_bool));
      ("integer?", procedure (Function1 is_integer));
      ("float?", procedure (Function1 is_float));
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
      ("gensym", Null);
      ("start-timer!", Null);
      ("stop-timer!", Null);
      ("char?", procedure (Function1 is_char));
      ("string?", procedure (Function1 is_string));
      ("string-length", procedure (Function1 string_length));
      ("substring", procedure (Function3 substring));
      ("string->list", procedure (Function1 string_to_list));
      ("list->string", procedure (Function1 list_to_string));
      ("string-append", procedure (Function2 string_append));
      ("vector", procedure (FunctionVariadic vector));
      ("make-vector", procedure (Function2 make_vector));
      ("vector?", procedure (Function1 is_vector));
      ("vector-length", procedure (Function1 vector_length));
      ("vector-ref", procedure (Function2 vector_ref));
      ("vector-set!", procedure (Function3 vector_set));
      ("vector->list", procedure (Function1 vector_to_list));
      ("list->vector", procedure (Function1 list_to_vector));
      ("make-hashtable", procedure (Function1 make_hashtable));
      ("hashtable?", procedure (Function1 is_hashtable));
      ("hashtable-size", procedure (Function1 hashtable_size));
      ("hashtable-keys", procedure (Function1 hashtable_keys));
      ("hashtable-ref", procedure (Function3 hashtable_ref));
      ("hashtable-set!", procedure (Function3 hashtable_set));
      ("hashtable-delete!", procedure (Function2 hashtable_delete)) ]

let definitions =
  List.fold_left
    (fun names (name, _) -> Symbol.Set.add name names)
    Symbol.Set.empty objects

let make_scope () =
  List.fold_left
    (fun scope (name, obj) -> Scope.add name (ref obj) scope)
    Scope.empty objects
