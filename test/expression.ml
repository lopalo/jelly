(* TODO: use "show" and "eq"(???) from "ppx_deriving"  *)

module Expr = Jelly.Expression

type symbol = Expr.symbol = Symbol of string [@@unboxed] [@@deriving show, eq]

type arguments = Expr.arguments =
  | Fixed of Expr.symbol list
  | Variadic of Expr.symbol
[@@deriving show, eq]

(* TODO: add meta *)
type 'a t = 'a Expr.t =
  | Value of 'a
  | Identifier of Expr.symbol
  | If of
      { condition : 'a t;
        then_expr : 'a t;
        else_expr : 'a t }
  | Lambda of
      { arguments : arguments;
        closure_names : Expr.SymbolSet.t;
        expressions : 'a t list }
  | Application of
      { expressions : 'a t list;
        evaluated_expressions : 'a t list }
  | Define of
      { name : Expr.symbol;
        expression : 'a t }
  | Set of
      { name : Expr.symbol;
        expression : 'a t }
[@@deriving show]
