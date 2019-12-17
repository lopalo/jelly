let format = Printf.sprintf

module Obj = Jelly.Object

let parse_file file_name =
  let str = Jelly.Common.read_file file_name in
  match Jelly.Parser.parse_lisp str with
  | Ok objs -> Some objs
  | Error
      (ParseError ({line_number = lnum; column_number = cnum; input; _}, msg))
    ->
      let prefix = String.sub input 0 @@ min 50 @@ String.length input in
      print_endline
      @@ format "Parse error at line %i column %i: %s\n\n%s" (succ lnum)
           (succ cnum) msg prefix;
      None

let () =
  let pretty = ref false in
  let print_objects objs =
    List.iter
      (fun obj ->
        if !pretty then (
          Obj.formatter Fmt.stdout obj;
          Format.pp_force_newline Fmt.stdout ())
        else Obj.to_string obj |> print_endline;
        print_newline ())
      objs
  in
  let handler file_name =
    ignore @@ Option.map print_objects (parse_file file_name);
    ()
  in
  let specs = [("-pretty", Arg.Set pretty, "Pretty-print output objects")] in
  let usage = "jelly [options] file" in
  Arg.parse specs handler usage
