module M = struct
  type t = Symbol of string [@@unboxed] [@@deriving show, eq]

  let compare (Symbol s) (Symbol s') = String.compare s s'
end

include M
module Set = Common.MakeSet (M)
module Map = Map.Make (M)
