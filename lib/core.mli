val definitions : Symbol.Set.t

val procedure : Object.func -> Object.t

val bad_arg : string -> Object.t -> ('a, string) result

val bad_args : string -> Object.t list -> ('a, string) result

val make_scope : unit -> Object.t ref Symbol.Map.t
