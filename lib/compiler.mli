type t = Object.t Expression.t

type error =
  | InvalidForm of Object.t
  | UndefinedName of Expression.Symbol.t * Common.meta option
  | DuplicateLocalDefinition of Expression.Symbol.t * Common.meta option

val pp_error : error Fmt.t

val show_error : error -> string

val equal_error : error -> error -> bool

val compile_top_level :
  Expression.SymbolSet.t -> Object.t -> (t list, error) result

val compile_top_level_lambda : Object.t list -> (t, error) result
