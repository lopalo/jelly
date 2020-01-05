open Expression
module Scope = SymbolMap
module Obj = Object

let format = Printf.sprintf

type error =
  (* TODO *)
  (* CompilationError of Compiler.error *)
  | RuntimeError of
      { error : string;
        stack_size : int;
        stack_trace : Common.meta list }
[@@deriving show, eq]

type context = {mutable error_handler : Obj.t option}

let function_arguments_number = function
  | Obj.FunctionVariadic _ -> "*"
  | Function0 _ -> "0"
  | Function1 _ -> "1"
  | Function2 _ -> "2"
  | Function3 _ -> "3"

let closure_arguments_number = function
  | Fixed names -> List.length names |> string_of_int
  | Variadic _ -> "*"

let rec execute ctx (stack : Obj.stack) scope = function
  | Value obj -> return ctx stack obj
  | Identifier {name; _} -> Scope.find name scope |> ( ! ) |> return ctx stack
  | If {condition; _} as e ->
      execute ctx ({expression = e; scope} :: stack) scope condition
  | Lambda ({closure_names; _} as e) ->
      let closure_scope =
        Scope.filter (fun x _ -> SymbolSet.mem x closure_names) scope
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
          | Ok scope -> execute_lambda ctx stack scope lambda
          | Error error -> application_error ctx stack meta error))
    | _ ->
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
    | _ -> scope
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
  | None ->
      let lambda_meta (stack_frame : Obj.stack_frame) =
        match stack_frame.expression with
        | Lambda {meta; _} -> meta
        | _ -> None
      in
      let stack_trace = List.filter_map lambda_meta stack in
      let stack_trace =
        match meta with
        | Some m -> m :: stack_trace
        | None -> stack_trace
      in
      Error (RuntimeError {error; stack_size = List.length stack; stack_trace})

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
        return ctx stack obj)

let execute_top_level expression =
  let ctx = {error_handler = None} in
  let set_error_handler =
    Core.procedure
      (Function1
         (function
         | (Procedure (Closure {lambda = {arguments = Fixed [_]; _}; _}) as
           proc)
         (* TODO *)
         (* | Procedure Continuation *)
         
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
  let scope = Core.make_scope () in
  Scope.find (Symbol.Symbol "set-error-handler!") scope := set_error_handler;
  Scope.find (Symbol.Symbol "reset-error-handler!") scope
  := reset_error_handler;
  let expression =
    Application {expressions = [expression]; computed_values = []; meta = None}
  in
  execute ctx [] scope expression

let scope_to_definitions scope =
  Scope.fold
    (fun name _ names -> SymbolSet.add name names)
    scope SymbolSet.empty
