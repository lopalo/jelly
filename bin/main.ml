let format = Printf.sprintf

module Obj = Jelly.Object

type meta = Jelly.Common.meta

let parse_string source_name str =
  match Jelly.Parser.parse_lisp ~source_name str with
  | Ok objs -> Some objs
  | Error
      (ParseError ({line_number = lnum; column_number = cnum; input; _}, msg))
    ->
      let prefix = String.sub input 0 @@ min 50 @@ String.length input in
      print_endline
      @@ format "Parse error at line %i column %i of '%s': %s\n\n%s"
           (succ lnum) (succ cnum) source_name msg prefix;
      None

let parse_file file_name =
  Jelly.Common.read_file file_name |> parse_string file_name

let print_quoted str = format "'%s'" str |> print_string

let print_meta = function
  | Some ({line_number; column_number; source_name} : meta) ->
      format " at line %i column %i of '%s' " (succ line_number)
        (succ column_number) source_name
      |> print_string
  | None -> ()

let execute objects =
  match Jelly.Runtime.execute_top_level objects with
  | Ok obj -> Some obj
  | Error err ->
      (match err with
      | Compilation err -> (
        match err with
        | InvalidForm obj ->
            print_string "Invalid form ";
            Obj.to_string obj |> print_quoted;
            Obj.meta obj |> print_meta
        | UndefinedName (Symbol name, meta) ->
            print_string "Undefined name ";
            print_quoted name;
            print_meta meta
        | DuplicateLocalDefinition (Symbol name, meta) ->
            print_string "Duplicate local definition ";
            print_quoted name;
            print_meta meta)
      | Runtime (RuntimeError {error; stack_trace; _}) ->
          print_string "Runtime error: ";
          print_endline error;
          List.iter
            (fun meta -> print_meta (Some meta); print_newline ())
            stack_trace
      | SyntaxExpansion (SyntaxExpansionError {error; stack_trace; _}) ->
          print_string "Syntax expansion error: ";
          print_endline error;
          List.iter
            (fun meta -> print_meta (Some meta); print_newline ())
            stack_trace);
      print_newline (); None

let () =
  let pretty = ref false in
  let scripts = ref [] in
  let print_object obj =
    if !pretty then (
      Obj.pp Fmt.stdout obj;
      Format.pp_force_newline Fmt.stdout ())
    else Obj.to_string obj |> print_endline
  in
  let load file_name =
    Option.map (fun objs -> scripts := objs :: !scripts) (parse_file file_name)
    |> ignore
  in
  let handler str =
    Option.map
      (fun objs -> objs :: !scripts |> List.rev |> List.concat)
      (parse_string "command line" str)
    |> (Fun.flip Option.bind) execute
    |> Option.map print_object |> ignore
  in
  let specs =
    [ ("-pretty", Arg.Set pretty, "Pretty-print the result");
      ("-load", Arg.String load, "Load and execute a script") ]
  in
  let usage = "jelly [options] expressions" in
  Arg.parse specs handler usage
