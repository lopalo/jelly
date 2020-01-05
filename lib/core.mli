val definitions : Expression.SymbolSet.t

val procedure : Object.func -> Object.t

val bad_arg : string -> Object.t -> ('a, string) result

val make_scope : unit -> Object.t ref Expression.SymbolMap.t
