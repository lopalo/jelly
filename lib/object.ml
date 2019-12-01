type symbol = Symbol of string [@@unboxed]

(* TODO: PairWithMeta, SymWithMeta, HashTable, Record *)
type t =
  | Null
  | Bool of bool
  | Int of int
  | Float of float
  | Char of char
  | Str of string
  | Sym of symbol
  | Vec of t array
  | Pair of
      { mutable car : t;
        mutable cdr : t }
