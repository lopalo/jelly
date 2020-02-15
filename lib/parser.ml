module O = Option
module S = String
module L = List
module LL = ListLabels
module Obj = Object

let format = Printf.sprintf

type state =
  { input : string;
    offset : int;
    commited_offset : int;
    source_name : string;
    line_number : int;
    column_number : int }
[@@deriving show, eq]

type 'a parsed = Parsed of state * 'a

type error = ParseError of state * string [@@deriving show, eq]

let ok state value = Ok (Parsed (state, value))

let error state msg = Error (ParseError (state, msg))

let return value state = ok state value

let bind parser f state =
  match parser state with
  | Ok (Parsed (state', value)) -> f value state'
  | Error _ as err -> err

let ( let* ) = bind

let fmap f parser =
  let* r = parser in
  return (f r)

let current_position ({source_name; line_number; column_number; _} as state) =
  ok state ({source_name; line_number; column_number} : Common.position)

let chars_of_string string = L.init (S.length string) (S.get string)

let string_of_chars chars =
  let buf = Buffer.create @@ L.length chars in
  L.iter (Buffer.add_char buf) chars;
  Buffer.contents buf

let satisfy ?(pred_name = "unspecified") pred
    ({input; offset; source_name; line_number; column_number; _} as state) =
  if S.length input = 0 then error state "End of input is reached"
  else
    let char = input.[0] in
    if not (pred char) then
      error state @@ format "Predicate '%s' is not satisfied" pred_name
    else
      let input = S.sub input 1 (S.length input - 1) in
      let line_number, column_number =
        if char = '\n' then (succ line_number, 0)
        else (line_number, succ column_number)
      in
      ok
        { state with
          input;
          offset = succ offset;
          source_name;
          line_number;
          column_number }
        char

let any = satisfy @@ Fun.const true

let one_of chars =
  LL.mem ~set:chars |> satisfy ~pred_name:("one of: " ^ string_of_chars chars)

let none_of chars =
  LL.mem ~set:chars |> Fun.negate
  |> satisfy ~pred_name:("none of: " ^ string_of_chars chars)

let character c = one_of [c]

let dot = character '.'

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

let and_then ~join parser parser' =
  let* r = parser in
  let* r' = parser' in
  join r r' |> return

let first x _ = x

let second _ x = x

let or_else parser parser' state =
  match parser state with
  | Ok _ as r -> r
  | Error (ParseError (state', _)) as err ->
      if state.offset >= state'.commited_offset then parser' state else err

let commit ({offset; _} as state) = ok {state with commited_offset = offset} ()

let optional parser = or_else (fmap O.some parser) (return O.none)

let rec many parser = or_else (plus parser) (return [])

and plus parser =
  let* x = parser in
  let* xs = many parser in
  return (x :: xs)

let rec separated_many ~sep parser =
  or_else (separated_plus ~sep parser) (return [])

and separated_plus ~sep parser =
  let* _ = optional sep in
  let* x = parser in
  let* opt_sep = optional sep in
  match opt_sep with
  | Some _ -> fmap (L.cons x) (separated_many ~sep parser)
  | None -> return [x]

let alphanumeric = or_else letter digit

let string s =
  let char_parsers = chars_of_string s |> L.map character in
  L.fold_right (and_then ~join:L.cons) char_parsers (return [])
  |> fmap string_of_chars

let end_of_input ({input; _} as state) =
  if S.length input = 0 then ok state ()
  else error state "End of input is not reached"

let parse_all parser ?(source_name = "no source") input =
  let res =
    (and_then ~join:first parser end_of_input)
      { input;
        offset = 0;
        commited_offset = 0;
        source_name;
        line_number = 0;
        column_number = 0 }
  in
  match res with
  | Ok (Parsed (state, x)) -> Ok (state, x)
  | Error _ as err -> err

let comment =
  let* c = character ';' in
  let* _ = many @@ none_of ['\n'] in
  return c

let lisp_space = or_else space comment

let lisp_spaces = plus lisp_space

let null = fmap (Fun.const Obj.Null) (or_else (string "()") (string "[]"))

let boolean =
  let* s = or_else (string "true") (string "false") in
  return @@ Obj.Bool (s = "true")

let sign minus =
  let* opt_sign = optional @@ one_of ['+'; '-'] in
  (match opt_sign with
  | Some '-' -> minus
  | _ -> Fun.id)
  |> return

let integer =
  let* sign = sign ( ~- ) in
  let obj i = Obj.Int i in
  let* ds = plus digit in
  string_of_chars ds |> int_of_string |> sign |> obj |> return

let floating =
  let* sign = sign ( ~-. ) in
  let obj i = Obj.Float i in
  let* ds = plus digit in
  let* dot = dot in
  let* ds' = plus digit in
  let ds'' = ds @ (dot :: ds') in
  string_of_chars ds'' |> float_of_string |> sign |> obj |> return

let escaped_character = and_then ~join:second (character '\\') any

let char_literal =
  let space = fmap (Fun.const ' ') (string "\\space") in
  let newline = fmap (Fun.const '\n') (string "\\newline") in
  fmap
    (fun x -> Obj.Char x)
    (or_else space @@ or_else newline escaped_character)

let string_literal =
  let d_char = '"' in
  let delimeter = character d_char in
  let* _ = delimeter in
  let* chars = many @@ or_else escaped_character @@ none_of [d_char] in
  let* _ = delimeter in
  return @@ Obj.Str (string_of_chars chars)

let symbol =
  let init =
    or_else letter
    @@ one_of
         [ '+';
           '-';
           '!';
           '$';
           '%';
           '&';
           '*';
           '/';
           ':';
           '<';
           '=';
           '>';
           '?';
           '~';
           '_';
           '^' ]
  in
  let subs = many @@ or_else init @@ or_else digit dot in
  let* pos = current_position in
  let* c = init in
  let* cs = if c = '+' || c = '-' then return [] else subs in
  let obj s = Obj.symbol ~meta:pos s in
  c :: cs |> string_of_chars |> obj |> return

let rec list_literal () =
  let* pos = current_position in
  let* d_char = one_of ['('; '['] in
  let* _ = commit in
  let* content = separated_plus ~sep:lisp_spaces (lisp ()) in
  let* _ = character (if d_char = '(' then ')' else ']') in
  let* _ = commit in
  return @@ Obj.list ~meta:pos content

and quote () =
  let* pos = current_position in
  let* _ = character '\'' in
  let* obj = lisp () in
  return @@ Obj.list ~meta:pos [Obj.symbol "quote"; obj]

and list_builder () =
  let* pos = current_position in
  let* _ = character '@' in
  let* obj = list_literal () in
  (match obj with
  | Cons (objs, _) -> Obj.list ~meta:pos (Obj.symbol "list" :: objs)
  | obj -> obj)
  |> return

and lisp () =
  and_then ~join:first
    (L.fold_left or_else null
       [ boolean;
         floating;
         integer;
         char_literal;
         string_literal;
         symbol;
         list_literal ();
         quote ();
         list_builder () ])
    commit

let lisp_parser = lisp () |> separated_many ~sep:lisp_spaces |> parse_all

let result_map = Stdlib.Result.map (* to avoild conflict with ppx rewriters *)

let parse_lisp ?source_name str =
  lisp_parser ?source_name str |> result_map snd

let sentences =
  let word = fmap string_of_chars (plus alphanumeric) in
  let sentence = separated_plus ~sep:(plus space) word in
  separated_many ~sep:dot sentence

let parse_sentences str = parse_all sentences str |> result_map snd
