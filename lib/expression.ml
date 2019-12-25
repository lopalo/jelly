module Symbol = struct
  type t = Symbol of string [@@unboxed] [@@deriving show, eq]

  let compare (Symbol s) (Symbol s') = String.compare s s'
end

module SymbolSet = Common.MakeSet (Symbol)

type arguments =
  | Fixed of Symbol.t list
  | Variadic of Symbol.t
[@@deriving show, eq]

type 'a t =
  | Value of 'a
  | Identifier of Symbol.t * Common.meta option
  | If of
      { condition : 'a t;
        then_expr : 'a t;
        else_expr : 'a t }
  | Lambda of
      { arguments : arguments;
        closure_names : SymbolSet.t;
        expressions : 'a t list;
        meta : Common.meta option }
  | Application of
      { expressions : 'a t list;
        evaluated_expressions : 'a t list;
        meta : Common.meta option }
  | Define of
      { name : Symbol.t;
        expression : 'a t;
        meta : Common.meta option }
  | Set of
      { name : Symbol.t;
        expression : 'a t;
        meta : Common.meta option }
[@@deriving show, eq]

(* TODO *)
(* | DefineSyntax of *)
(*     { name : symbol; *)
(*       expression : 'a t } *)
