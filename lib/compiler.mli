type t = Object.t Expression.t

type error =
  [ `InvalidForm of Object.t
  | `UndefinedName of Symbol.t * Common.meta option
  | `DuplicateDefinition of Symbol.t * Common.meta option ]

val pp_error : error Fmt.t

val show_error : error -> string

val equal_error : error -> error -> bool

val compile_top_level : Symbol.Set.t -> Object.t -> (t list, error) result

val compile_top_level_lambda : Object.t list -> (t, error) result
