let format = Printf.sprintf

module Obj = Jelly.Object

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

let () =
  let pretty = ref false in
  let scripts = ref [] in
  let print_objects objs =
    List.iter
      (fun obj ->
        if !pretty then (
          Obj.pp Fmt.stdout obj;
          Format.pp_force_newline Fmt.stdout ())
        else Obj.to_string obj |> print_endline;
        print_newline ())
      objs
  in
  (* TODO: pretty-print the result of the last executed expression *)
  let load file_name =
    ignore
    @@ Option.map
         (fun objs -> scripts := objs :: !scripts)
         (parse_file file_name)
  in
  let handler str =
    ignore
    @@ Option.map
         (fun objs ->
           objs :: !scripts |> List.rev |> List.concat |> print_objects)
         (parse_string "command line" str)
  in
  let specs =
    [ ("-pretty", Arg.Set pretty, "Pretty-print the result");
      ("-load", Arg.String load, "Load and execute a script") ]
  in
  let usage = "jelly [options] expressions" in
  Arg.parse specs handler usage
