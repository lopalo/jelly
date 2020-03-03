type error_context =
  { error : string;
    stack_size : int;
    stack_trace : Common.meta list }

type error =
  [ Compiler.error
  | `RuntimeError of error_context
  | `SyntaxExpansionError of error_context ]

val pp_error : error Fmt.t

val show_error : error -> string

val equal_error : error -> error -> bool

val execute_top_level : Object.t list -> (Object.t, error) result

val scope_to_definitions : 'a Symbol.Map.t -> Symbol.Set.t
