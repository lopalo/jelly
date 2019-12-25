type position =
  { source_name : string;
    line_number : int;
    column_number : int }
[@@deriving show, eq]

type meta = position [@@deriving show, eq]

module type SetElement = sig
  include Set.OrderedType

  val pp : t Fmt.t
end

module MakeSet (Element : SetElement) = struct
  include Set.Make (Element)

  let pp ppf v =
    Fmt.braces (Fmt.list ~sep:Fmt.semi (Fmt.box Element.pp)) ppf (elements v)
end

let read_file file_name =
  let ch = open_in file_name in
  let str = really_input_string ch (in_channel_length ch) in
  close_in ch; str
