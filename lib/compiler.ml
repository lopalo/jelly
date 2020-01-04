module Symbol = Expression.Symbol
module SymbolSet = Expression.SymbolSet

type t = Object.t Expression.t

exception InvalidFormExn of Object.t

let meta = function
  | Object.Cons (_, meta) -> meta
  | _ -> None

let rec compile ?(top_level = false) ?(lambda_level = false) obj =
  match obj with
  | Object.Null
  | Bool _
  | Int _
  | Float _
  | Char _
  | Str _ ->
      Expression.Value obj
  | Sym (name, meta) -> Identifier {name; meta}
  | Cons (objs, meta) -> (
    match objs with
    | [] -> Value Null
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
        | _ -> compile_application meta objs)
      | Cons _ -> compile_application meta objs
      | _ -> raise (InvalidFormExn obj)))
  | Procedure _ -> raise (InvalidFormExn obj)

and compile_quote obj = function
  | [obj] -> Value obj
  | _ -> raise (InvalidFormExn obj)

and compile_if obj = function
  | [condition; then_form; else_form] ->
      If
        { condition = compile condition;
          then_expr = compile then_form;
          else_expr = compile else_form }
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
      Lambda
        { arguments;
          closure_names = SymbolSet.empty;
          expressions =
            List.map (compile ~lambda_level:true) (first_form :: rest_forms);
          meta = meta obj }
  | _ -> raise (InvalidFormExn obj)

and compile_application meta objs =
  Application {expressions = List.map compile objs; computed_values = []; meta}

and compile_define ~lambda_level obj args =
  match (lambda_level, args) with
  | true, [Sym (name, _); form] ->
      Define {name; expression = compile form; meta = meta obj}
  | _ -> raise (InvalidFormExn obj)

and compile_set obj = function
  | [Sym (name, _); form] ->
      Set {name; expression = compile form; meta = meta obj}
  | _ -> raise (InvalidFormExn obj)

and compile_define_syntax ~top_level obj args =
  match (top_level, args) with
  (* TODO: only for top_level = true *)
  | _ -> raise (InvalidFormExn obj)

exception UndefinedNameExn of Symbol.t * Common.meta option

exception DuplicateLocalDefinitionExn of Symbol.t * Common.meta option

let rec resolve_names ~definitions ~local_definitions ~local_names = function
  | [] -> ([], SymbolSet.diff local_names local_definitions)
  | expression :: expressions -> (
    match expression with
    | Expression.Value _ ->
        let expressions, outer_names =
          resolve_names ~definitions ~local_definitions ~local_names
            expressions
        in
        (expression :: expressions, outer_names)
    | Identifier {name; meta} ->
        if not (SymbolSet.mem name definitions) then
          raise (UndefinedNameExn (name, meta));
        let local_names = SymbolSet.add name local_names in
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
        let local_names = SymbolSet.union local_names expr_outer_names in
        let expressions, outer_names =
          resolve_names ~definitions ~local_definitions ~local_names
            expressions
        in
        (If {condition; then_expr; else_expr} :: expressions, outer_names)
    | Lambda ({arguments; expressions = lambda_exprs; _} as e) ->
        let arg_names =
          match arguments with
          | Fixed names -> SymbolSet.of_list names
          | Variadic name -> SymbolSet.singleton name
        in
        let lambda_exprs, expr_outer_names =
          resolve_names
            ~definitions:(SymbolSet.union arg_names definitions)
            ~local_definitions:arg_names ~local_names:SymbolSet.empty
            lambda_exprs
        in
        let local_names = SymbolSet.union local_names expr_outer_names in
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
        let local_names = SymbolSet.union local_names expr_outer_names in
        let expressions, outer_names =
          resolve_names ~definitions ~local_definitions ~local_names
            expressions
        in
        ( Application {e with expressions = application_exprs} :: expressions,
          outer_names )
    | Define ({name; expression = define_expr; meta} as e) ->
        if SymbolSet.mem name local_definitions then
          raise (DuplicateLocalDefinitionExn (name, meta));
        let definitions = SymbolSet.add name definitions in
        let local_definitions = SymbolSet.add name local_definitions in
        let[@warning "-8"] [define_expr], expr_outer_names =
          resolve_names ~definitions ~local_definitions ~local_names
            [define_expr]
        in
        let local_names = SymbolSet.union local_names expr_outer_names in
        let expressions, outer_names =
          resolve_names ~definitions ~local_definitions ~local_names
            expressions
        in
        (Define {e with expression = define_expr} :: expressions, outer_names)
    | Set ({name; expression = set_expr; meta} as e) ->
        if not (SymbolSet.mem name definitions) then
          raise (UndefinedNameExn (name, meta));
        let[@warning "-8"] [set_expr], expr_outer_names =
          resolve_names ~definitions ~local_definitions ~local_names [set_expr]
        in
        let local_names = SymbolSet.union local_names expr_outer_names in
        let expressions, outer_names =
          resolve_names ~definitions ~local_definitions ~local_names
            expressions
        in
        (Set {e with expression = set_expr} :: expressions, outer_names))

type error =
  | InvalidForm of Object.t
  | UndefinedName of Symbol.t * Common.meta option
  | DuplicateLocalDefinition of Symbol.t * Common.meta option
[@@deriving show, eq]

let compile_top_level objs =
  let open Expression in
  try
    let expressions =
      List.map (compile ~top_level:true ~lambda_level:true) objs
    in
    let empty = SymbolSet.empty in
    let expressions, outer_names =
      resolve_names ~definitions:Core.definitions ~local_definitions:empty
        ~local_names:empty expressions
    in
    Ok
      (Lambda
         { arguments = Fixed [];
           closure_names = outer_names;
           expressions;
           meta = None })
  with
  | InvalidFormExn o -> Error (InvalidForm o)
  | UndefinedNameExn (s, m) -> Error (UndefinedName (s, m))
  | DuplicateLocalDefinitionExn (s, m) ->
      Error (DuplicateLocalDefinition (s, m))
