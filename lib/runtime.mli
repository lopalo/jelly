type error =
  (* TODO *)
  (* CompilationError of Compiler.error *)
  | RuntimeError of
      { error : string;
        stack_trace : Common.meta list }

val pp_error : error Fmt.t

val show_error : error -> string

val equal_error : error -> error -> bool

val execute_top_level : Object.expression -> (Object.t, error) result

val scope_to_definitions : 'a Expression.SymbolMap.t -> Expression.SymbolSet.t
