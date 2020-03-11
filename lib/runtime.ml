open Expression
module Scope = Symbol.Map
module Obj = Object

let format = Printf.sprintf

type error_context =
  { error : string;
    stack_size : int;
    stack_trace : Common.meta list }
[@@deriving show, eq]

type error =
  [ Compiler.error
  | `RuntimeError of error_context
  | `SyntaxExpansionError of error_context ]
[@@deriving show, eq]

type timer =
  { mutable ticks : int;
    handler : Obj.t }

type context =
  { mutable syntax_scope : Obj.scope;
    mutable gensym_count : int;
    mutable error_handler : Obj.t option;
    mutable timer : timer option }

let function_arguments_number = function
  | Obj.FunctionVariadic _ -> "*"
  | Function0 _ -> "0"
  | Function1 _ -> "1"
  | Function2 _ -> "2"
  | Function3 _ -> "3"

let closure_arguments_number = function
  | Fixed names -> List.length names |> string_of_int
  | Variadic _ -> "*"

exception SyntaxExpansionExn of error_context

let error_context stack meta error =
  let frame_meta (stack_frame : Obj.stack_frame) =
    match stack_frame.expression with
    | Lambda {meta; _}
    | Application {meta; _} ->
        meta
    | Value _
    | Identifier _
    | If _
    | Define _
    | Set _
    | DefineSyntax _ ->
        None
  in
  let stack_trace = List.filter_map frame_meta stack in
  let stack_trace =
    match meta with
    | Some m -> m :: stack_trace
    | None -> stack_trace
  in
  {error; stack_size = List.length stack; stack_trace}

let rec execute ctx (stack : Obj.stack) scope = function
  | Value obj -> return ctx stack obj
  | Identifier {name; _} -> Scope.find name scope |> ( ! ) |> return ctx stack
  | If {condition; _} as e ->
      execute ctx ({expression = e; scope} :: stack) scope condition
  | Lambda ({closure_names; _} as e) ->
      let closure_scope =
        Scope.filter (fun x _ -> Symbol.Set.mem x closure_names) scope
      in
      Obj.Procedure (Obj.Closure {lambda = e; scope = closure_scope})
      |> return ctx stack
  | Application ({expressions; _} as e) -> (
    match expressions with
    | [] -> return ctx stack Obj.Null
    | operator :: expressions ->
        execute ctx
          ({expression = Application {e with expressions}; scope} :: stack)
          scope operator)
  | (Define {expression; _} as e)
  | (DefineSyntax {expression; _} as e)
  | (Set {expression; _} as e) ->
      execute ctx ({expression = e; scope} :: stack) scope expression

and execute_procedure ctx stack meta = function
  | [] -> return ctx stack Obj.Null
  | obj :: args -> (
    match obj with
    | Obj.Procedure proc -> (
      match proc with
      | Function func -> (
        match execute_function (func, args) with
        | Ok obj -> return ctx stack obj
        | Error error -> application_error ctx stack meta error)
      | Closure {lambda = {arguments; _} as lambda; scope} -> (
          let scope_res =
            match arguments with
            | Fixed names ->
                if List.length names <> List.length args then
                  Error
                    (format "Closure takes %s arguments"
                       (closure_arguments_number arguments))
                else
                  Ok
                    (List.fold_left2
                       (fun scope name arg -> Scope.add name (ref arg) scope)
                       scope names args)
            | Variadic name -> Ok (Scope.add name (ref (Obj.list args)) scope)
          in
          match scope_res with
          | Ok scope -> (
            match ctx.timer with
            | None -> execute_lambda ctx stack scope lambda
            | Some timer ->
                timer.ticks <- timer.ticks - 1;
                if timer.ticks > 0 then execute_lambda ctx stack scope lambda
                else (
                  ctx.timer <- None;
                  let (stack : Obj.stack) =
                    {expression = Lambda lambda; scope} :: stack
                  in
                  execute_procedure ctx stack meta [timer.handler]))
          | Error error -> application_error ctx stack meta error)
      | Apply -> (
        match args with
        | [(Procedure _ as proc); Cons (arg_list, _)] ->
            execute_procedure ctx stack meta (proc :: arg_list)
        | _ ->
            application_error ctx stack meta
              "'apply' function takes a procedure and a list of arguments")
      | SyntaxExpander -> (
        match args with
        | [arg] -> (
            let timer = ctx.timer in
            ctx.timer <- None;
            let err_handler = ctx.error_handler in
            ctx.error_handler <- None;
            let res = expand_syntax ctx arg in
            ctx.error_handler <- err_handler;
            ctx.timer <- timer;
            match res with
            | Ok obj -> return ctx stack obj
            | Error (`SyntaxExpansionError {error; stack_size; stack_trace})
              -> (
              match err_handler with
              | Some proc ->
                  execute_procedure ctx stack meta [proc; Obj.Str error]
              | None ->
                  let err_ctx = error_context stack meta "" in
                  Error
                    (`RuntimeError
                      { error;
                        stack_size = stack_size + err_ctx.stack_size;
                        stack_trace = stack_trace @ err_ctx.stack_trace })))
        | _ ->
            application_error ctx stack meta "Syntax expander takes 1 argument"
        )
      | CallWithCurrentContinuation -> (
        match args with
        | [(Procedure _ as proc)] ->
            execute_procedure ctx stack meta
              [proc; Procedure (Continuation stack)]
        | _ ->
            application_error ctx stack meta
              "'call/cc' function takes a procedure")
      | Continuation next_stack -> (
        match args with
        | [arg] -> return ctx next_stack arg
        | _ -> application_error ctx stack meta "Continuation takes 1 argument"
        ))
    | obj ->
        application_error ctx stack meta
          (Obj.to_string obj ^ " is not a procedure"))

and execute_function = function
  | FunctionVariadic f, args -> f args
  | Function0 f, [] -> f ()
  | Function1 f, [a] -> f a
  | Function2 f, [a; b] -> f a b
  | Function3 f, [a; b; c] -> f a b c
  | func, _ ->
      Error
        (format "Function takes %s arguments" (function_arguments_number func))

and execute_lambda ctx stack scope ({expressions; _} as lambda : Obj.lambda) =
  let make_scope = function
    | Define {name; _} -> Scope.add name (ref Obj.Null) scope
    | Lambda _
    | Value _
    | Identifier _
    | If _
    | Application _
    | Set _
    | DefineSyntax _ ->
        scope
  in
  match expressions with
  | [] -> failwith "Tail call is not eliminated"
  | [expr] -> execute ctx stack (make_scope expr) expr
  | expr :: expressions ->
      let scope = make_scope expr in
      execute ctx
        ({expression = Lambda {lambda with expressions}; scope} :: stack)
        scope expr

and application_error ctx stack meta error =
  match ctx.error_handler with
  | Some proc -> execute_procedure ctx stack meta [proc; Obj.Str error]
  | None -> Error (`RuntimeError (error_context stack meta error))

and return ctx stack obj =
  match stack with
  | [] -> Ok obj
  | {expression; scope} :: stack -> (
    match expression with
    | Value _ -> failwith "Cannot return to a value"
    | Identifier _ -> failwith "Cannot return to an identifier"
    | If {then_expr; else_expr; _} ->
        (if Obj.is_true obj then then_expr else else_expr)
        |> execute ctx stack scope
    | Lambda lambda -> execute_lambda ctx stack scope lambda
    | Application ({expressions; computed_values; meta} as e) -> (
        let computed_values = obj :: computed_values in
        match expressions with
        | [] -> execute_procedure ctx stack meta @@ List.rev computed_values
        | argument :: expressions ->
            execute ctx
              ({ expression = Application {e with computed_values; expressions};
                 scope }
              :: stack)
              scope argument)
    | Define {name; _}
    | Set {name; _} ->
        Scope.find name scope := obj;
        return ctx stack obj
    | DefineSyntax {name; _} ->
        Scope.find name ctx.syntax_scope := obj;
        return ctx stack obj)

and expand_syntax ctx =
  let scope = ctx.syntax_scope in
  let rec traverse = function
    | Obj.Cons (Sym (name, _) :: objs, meta) as form when Scope.mem name scope
      -> (
      match
        execute_procedure ctx [] meta (!(Scope.find name scope) :: form :: objs)
      with
      | Ok obj -> traverse obj
      | Error (`RuntimeError error_context) ->
          raise (SyntaxExpansionExn error_context))
    | Cons (objs, meta) as obj -> (
      match objs with
      | Sym (Symbol "quote", _) :: _ -> obj
      | objs -> Obj.Cons (List.map traverse objs, meta))
    | obj -> obj
  in
  fun obj ->
    try Ok (traverse obj)
    with SyntaxExpansionExn error_context ->
      Error (`SyntaxExpansionError error_context)

let[@inline always] coerce_error result = (result :> (_, error) result)

let rec execute_top_level_objects ctx definitions scope result_obj = function
  | [] -> Ok result_obj
  | obj :: objects -> (
    match expand_syntax ctx obj with
    | Ok obj -> (
      match Compiler.compile_top_level definitions obj with
      | Ok expressions ->
          execute_expressions ctx definitions scope objects (Ok result_obj)
            expressions
      | Error _ as err -> coerce_error err)
    | Error _ as err -> coerce_error err)

and execute_expressions ctx definitions scope objects result expressions =
  match (result, expressions) with
  | Ok result_obj, [] ->
      execute_top_level_objects ctx definitions scope result_obj objects
  | (Error _ as err), _ -> err
  | _, expression :: expressions -> (
    match expression with
    | Define {name; meta; _} ->
        if Scope.mem name ctx.syntax_scope then
          Error (`DuplicateDefinition (name, meta))
        else
          let scope = Scope.add name (ref Obj.Null) scope in
          let definitions = Symbol.Set.add name definitions in
          let result = execute ctx [] scope expression |> coerce_error in
          execute_expressions ctx definitions scope objects result expressions
    | DefineSyntax {name; meta; _} ->
        if Scope.mem name ctx.syntax_scope then
          Error (`DuplicateDefinition (name, meta))
        else (
          ctx.syntax_scope <- Scope.add name (ref Obj.Null) ctx.syntax_scope;
          let result = execute ctx [] scope expression |> coerce_error in
          execute_expressions ctx definitions scope objects result expressions)
    | expression ->
        let result = execute ctx [] scope expression |> coerce_error in
        execute_expressions ctx definitions scope objects result expressions)

let scope_to_definitions scope =
  Scope.fold
    (fun name _ names -> Symbol.Set.add name names)
    scope Symbol.Set.empty

let execute_top_level objects =
  let ctx =
    { error_handler = None;
      syntax_scope = Scope.empty;
      gensym_count = 0;
      timer = None }
  in
  let set_error_handler =
    Core.procedure
      (Function1
         (function
         | (Procedure (Closure {lambda = {arguments = Fixed [_]; _}; _}) as
           proc)
         | (Procedure (Continuation _) as proc)
         | (Procedure (Function (Function1 _)) as proc) ->
             ctx.error_handler <- Some proc;
             Ok Obj.Null
         | o -> Core.bad_arg "set-error-handler!.must-be-one-arg-procedure" o))
  in
  let reset_error_handler =
    Core.procedure
      (Function0
         (fun () ->
           ctx.error_handler <- None;
           Ok Obj.Null))
  in
  let gensym =
    Core.procedure
      (Function1
         (function
         | Str prefix ->
             let c = ctx.gensym_count in
             ctx.gensym_count <- succ c;
             Ok (Obj.Sym (symbol @@ prefix ^ string_of_int c, None))
         | o -> Core.bad_arg "gensym" o))
  in
  let start_timer =
    Core.procedure
      (Function2
         (fun ticks handler ->
           match (ticks, handler) with
           | ( Int ticks,
               Procedure (Closure {lambda = {arguments = Fixed []; _}; _}) )
           | Int ticks, Procedure (Function (Function0 _)) ->
               ctx.timer <- Some {ticks; handler};
               Ok Null
           | ticks, handler ->
               Core.bad_args "start-timer!.must-be-int-and-zero-arg-procedure"
                 [ticks; handler]))
  in
  let stop_timer =
    Core.procedure
      (Function0
         (fun () ->
           let timer = ctx.timer in
           ctx.timer <- None;
           Ok
             (match timer with
             | Some {ticks; _} -> Int ticks
             | None -> Null)))
  in
  let scope = Core.make_scope () in
  let definitions = scope_to_definitions scope in
  Scope.find (symbol "set-error-handler!") scope := set_error_handler;
  Scope.find (symbol "reset-error-handler!") scope := reset_error_handler;
  Scope.find (symbol "gensym") scope := gensym;
  Scope.find (symbol "start-timer!") scope := start_timer;
  Scope.find (symbol "stop-timer!") scope := stop_timer;
  execute_top_level_objects ctx definitions scope Obj.Null objects
