type t = Object.t Expression.t

exception InvalidFormExn of Object.t

let meta = Object.meta

let flat_map f lst = List.map f lst |> List.flatten

let rec compile ?(top_level = false) ?(lambda_level = false) = function
  | (Object.Null | Bool _ | Int _ | Float _ | Char _ | Str _) as obj ->
      [Expression.Value obj]
  | Sym (name, meta) -> [Identifier {name; meta}]
  | Cons (objs, meta) as obj -> (
    match objs with
    | [] -> [Value Null]
    | operator :: args -> (
      match operator with
      | Sym (Symbol name, _) -> (
        match name with
        | "quote" -> compile_quote obj args
        | "if" -> compile_if obj args
        | "lambda" -> compile_lambda obj args
        | "define" -> compile_define ~lambda_level obj args
        | "set!" -> compile_set obj args
        | "define-syntax" -> compile_define_syntax ~top_level obj args
        | "begin" -> flat_map (compile ~top_level ~lambda_level) args
        | _ -> compile_application meta objs)
      | Cons _ -> compile_application meta objs
      | _ -> raise (InvalidFormExn obj)))
  | Procedure _ as obj -> raise (InvalidFormExn obj)

and compile_one obj =
  match compile obj with
  | [expression] -> expression
  | _ -> raise (InvalidFormExn obj)

and compile_quote obj = function
  | [obj] -> [Value obj]
  | _ -> raise (InvalidFormExn obj)

and compile_if obj = function
  | [condition; then_form; else_form] ->
      [ If
          { condition = compile_one condition;
            then_expr = compile_one then_form;
            else_expr = compile_one else_form } ]
  | _ -> raise (InvalidFormExn obj)

and compile_lambda obj = function
  | args :: first_form :: rest_forms ->
      let arguments =
        match args with
        | Null -> Expression.Fixed []
        | Sym (s, _) -> Expression.Variadic s
        | Cons (objs, _) ->
            Expression.Fixed
              (List.map
                 (function
                   | Object.Sym (s, _) -> s
                   | _ -> raise (InvalidFormExn obj))
                 objs)
        | _ -> raise (InvalidFormExn obj)
      in
      [ Lambda
          { arguments;
            closure_names = Symbol.Set.empty;
            expressions =
              flat_map (compile ~lambda_level:true) (first_form :: rest_forms);
            meta = meta obj } ]
  | _ -> raise (InvalidFormExn obj)

and compile_application meta objs =
  [ Application
      {expressions = flat_map compile objs; computed_values = []; meta} ]

and compile_define ~lambda_level obj args =
  match (lambda_level, args) with
  | true, [Sym (name, _); form] ->
      [Define {name; expression = compile_one form; meta = meta obj}]
  | _ -> raise (InvalidFormExn obj)

and compile_set obj = function
  | [Sym (name, _); form] ->
      [Set {name; expression = compile_one form; meta = meta obj}]
  | _ -> raise (InvalidFormExn obj)

and compile_define_syntax ~top_level obj args =
  match (top_level, args) with
  | true, [Sym (name, _); form] ->
      [DefineSyntax {name; expression = compile_one form; meta = meta obj}]
  | _ -> raise (InvalidFormExn obj)

exception UndefinedNameExn of Symbol.t * Common.meta option

exception DuplicateDefinitionExn of Symbol.t * Common.meta option

let rec resolve_names ~definitions ~local_definitions ~local_names = function
  | [] -> ([], Symbol.Set.diff local_names local_definitions)
  | expression :: expressions -> (
    match expression with
    | Expression.Value _ as expression ->
        let expressions, outer_names =
          resolve_names ~definitions ~local_definitions ~local_names
            expressions
        in
        (expression :: expressions, outer_names)
    | Identifier {name; meta} ->
        if not (Symbol.Set.mem name definitions) then
          raise (UndefinedNameExn (name, meta));
        let local_names = Symbol.Set.add name local_names in
        let expressions, outer_names =
          resolve_names ~definitions ~local_definitions ~local_names
            expressions
        in
        (expression :: expressions, outer_names)
    | If {condition; then_expr; else_expr} ->
        let[@warning "-8"] [condition; then_expr; else_expr], expr_outer_names
            =
          resolve_names ~definitions ~local_definitions ~local_names
            [condition; then_expr; else_expr]
        in
        let local_names = Symbol.Set.union local_names expr_outer_names in
        let expressions, outer_names =
          resolve_names ~definitions ~local_definitions ~local_names
            expressions
        in
        (If {condition; then_expr; else_expr} :: expressions, outer_names)
    | Lambda ({arguments; expressions = lambda_exprs; _} as e) ->
        let arg_names =
          match arguments with
          | Fixed names -> Symbol.Set.of_list names
          | Variadic name -> Symbol.Set.singleton name
        in
        let lambda_exprs, expr_outer_names =
          resolve_names
            ~definitions:(Symbol.Set.union arg_names definitions)
            ~local_definitions:arg_names ~local_names:Symbol.Set.empty
            lambda_exprs
        in
        let local_names = Symbol.Set.union local_names expr_outer_names in
        let expressions, outer_names =
          resolve_names ~definitions ~local_definitions ~local_names
            expressions
        in
        ( Lambda
            { e with
              expressions = lambda_exprs;
              closure_names = expr_outer_names }
          :: expressions,
          outer_names )
    | Application ({expressions = application_exprs; _} as e) ->
        let application_exprs, expr_outer_names =
          resolve_names ~definitions ~local_definitions ~local_names
            application_exprs
        in
        let local_names = Symbol.Set.union local_names expr_outer_names in
        let expressions, outer_names =
          resolve_names ~definitions ~local_definitions ~local_names
            expressions
        in
        ( Application {e with expressions = application_exprs} :: expressions,
          outer_names )
    | Define ({name; expression = define_expr; meta} as e) ->
        if Symbol.Set.mem name local_definitions then
          raise (DuplicateDefinitionExn (name, meta));
        let definitions = Symbol.Set.add name definitions in
        let local_definitions = Symbol.Set.add name local_definitions in
        let[@warning "-8"] [define_expr], expr_outer_names =
          resolve_names ~definitions ~local_definitions ~local_names
            [define_expr]
        in
        let local_names = Symbol.Set.union local_names expr_outer_names in
        let expressions, outer_names =
          resolve_names ~definitions ~local_definitions ~local_names
            expressions
        in
        (Define {e with expression = define_expr} :: expressions, outer_names)
    | Set ({name; expression = set_expr; meta} as e) ->
        if not (Symbol.Set.mem name definitions) then
          raise (UndefinedNameExn (name, meta));
        let local_names = Symbol.Set.add name local_names in
        let[@warning "-8"] [set_expr], expr_outer_names =
          resolve_names ~definitions ~local_definitions ~local_names [set_expr]
        in
        let local_names = Symbol.Set.union local_names expr_outer_names in
        let expressions, outer_names =
          resolve_names ~definitions ~local_definitions ~local_names
            expressions
        in
        (Set {e with expression = set_expr} :: expressions, outer_names)
    | DefineSyntax ({name; expression = define_expr; meta} as e) ->
        if Symbol.Set.mem name local_definitions then
          raise (DuplicateDefinitionExn (name, meta));
        let[@warning "-8"] [define_expr], _ =
          resolve_names ~definitions ~local_definitions ~local_names
            [define_expr]
        in
        let expressions, outer_names =
          resolve_names ~definitions ~local_definitions ~local_names
            expressions
        in
        ( DefineSyntax {e with expression = define_expr} :: expressions,
          outer_names ))

type error =
  [ `InvalidForm of Object.t
  | `UndefinedName of Symbol.t * Common.meta option
  | `DuplicateDefinition of Symbol.t * Common.meta option ]
[@@deriving show, eq]

let try_top_level f x =
  try f x with
  | InvalidFormExn o -> Error (`InvalidForm o)
  | UndefinedNameExn (name, meta) -> Error (`UndefinedName (name, meta))
  | DuplicateDefinitionExn (name, meta) ->
      Error (`DuplicateDefinition (name, meta))

let compile_top_level definitions =
  try_top_level
  @@ fun obj ->
  let expressions = compile ~top_level:true ~lambda_level:true obj in
  let expressions, _ =
    resolve_names ~definitions ~local_definitions:definitions
      ~local_names:Symbol.Set.empty expressions
  in
  Ok expressions

let compile_top_level_lambda =
  try_top_level
  @@ fun objs ->
  let definitions = Core.definitions in
  let expressions =
    flat_map (compile ~top_level:false ~lambda_level:true) objs
  in
  let expressions, _ =
    resolve_names ~definitions ~local_definitions:definitions
      ~local_names:Symbol.Set.empty expressions
  in
  Ok
    (Expression.Lambda
       { arguments = Fixed [];
         closure_names = definitions;
         expressions;
         meta = None })
