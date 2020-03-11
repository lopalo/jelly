type state =
  { input : string;
    offset : int;
    commited_offset : int;
    source_name : string;
    line_number : int;
    column_number : int }

type error = ParseError of state * string

val pp_error : error Fmt.t

val show_error : error -> string

val equal_error : error -> error -> bool

val parse_lisp : ?source_name:string -> string -> (Object.t list, error) result

val parse_sentences : string -> (string list list, error) result

val string_of_chars : char list -> string
