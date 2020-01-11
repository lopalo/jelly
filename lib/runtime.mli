type error_context =
  { error : string;
    stack_size : int;
    stack_trace : Common.meta list }

type runtime_error = RuntimeError of error_context [@@unboxed]

type syntax_expansion_error = SyntaxExpansionError of error_context
[@@unboxed]

type error =
  | Compilation of Compiler.error
  | Runtime of runtime_error
  | SyntaxExpansion of syntax_expansion_error

val pp_error : error Fmt.t

val show_error : error -> string

val equal_error : error -> error -> bool

val execute_top_level : Object.t list -> (Object.t, error) result

val scope_to_definitions : 'a Expression.SymbolMap.t -> Expression.SymbolSet.t
