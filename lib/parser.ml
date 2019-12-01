module S = String
module L = List
module LL = ListLabels
module O = Object

let format = Printf.sprintf

type state =
  { input : string;
    offset : int;
    line_number : int;
    column_number : int }

type position =
  { line_number : int;
    column_number : int }

type error = ParseError of string * state

type ('a, 'b) parser =
  ('a -> state -> ('b, error) result) -> state -> ('b, error) result

let return value cont state = cont value state

let bind parser f cont state =
  parser (fun value state' -> (f value) cont state') state

let ( let* ) = bind

let fmap f parser =
  let* r = parser in
  return (f r)

let error msg state = Error (ParseError (msg, state))

let failure msg _ state = error msg state

let chars_of_string string = L.init (S.length string) (S.get string)

let string_of_chars chars =
  let buf = Buffer.create @@ L.length chars in
  L.iter (Buffer.add_char buf) chars;
  Buffer.contents buf

let any cont ({input; offset; line_number; column_number} as state) =
  if S.length input = 0 then error "End of input is reached" state
  else
    let char = input.[0] in
    let input' = S.sub input 1 (S.length input - 1) in
    let line_number', column_number' =
      if char = '\n' then (succ line_number, 0)
      else (line_number, succ column_number)
    in
    cont char
      { input = input';
        offset = succ offset;
        line_number = line_number';
        column_number = column_number' }

let satisfy ?(pred_name = "unspecified") pred =
  (fun value ->
    if pred value then return value
    else format "Predicate '%s' is not satisfied" pred_name |> failure)
  |> bind any

let one_of chars =
  LL.mem ~set:chars |> satisfy ~pred_name:("one of: " ^ string_of_chars chars)

let none_of chars =
  LL.mem ~set:chars |> Fun.negate
  |> satisfy ~pred_name:("none of: " ^ string_of_chars chars)

let letter =
  (function
    | 'a' .. 'z'
    | 'A' .. 'Z' ->
        true
    | _ -> false)
  |> satisfy ~pred_name:"letter"

let digit =
  (function
    | '0' .. '9' -> true
    | _ -> false)
  |> satisfy ~pred_name:"digit"

let space = one_of [' '; '\n'; ',']

let and_then ~combine parser parser' =
  let* r = parser in
  let* r' = parser' in
  combine r r' |> return

let or_else parser parser' cont state =
  match parser cont state with
  | Ok _ as r -> r
  | _ -> parser' cont state

let optional parser = [] |> return |> or_else parser

let rec many parser = parser |> plus |> optional

and plus parser =
  let* x = parser in
  let* xs = many parser in
  return (x :: xs)

let alphanumeric = or_else letter digit

let spaces = plus space

let string s =
  let char_parsers = s |> chars_of_string |> L.map (fun c -> one_of [c]) in
  L.fold_right (and_then ~combine:L.cons) char_parsers (return [])
  |> fmap string_of_chars

let end_of_input cont ({input; _} as state) =
  if S.length input = 0 then cont () state
  else error "End of input is not reached" state

let parse_all parser input =
  (and_then ~combine:(fun x _ -> x) parser end_of_input)
    (fun x state -> Ok (x, state))
    {input; offset = 0; line_number = 0; column_number = 0}

(* For testing *)

let word =
  let* chars = plus letter in
  let* _ = optional spaces in
  return @@ string_of_chars chars

let words =
  let* w = string "Jelly" in
  let* _ = spaces in
  let* ws = many word in
  return @@ (w :: ws)

let words_parser = parse_all words
