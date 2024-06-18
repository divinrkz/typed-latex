open Core

let string_sep str formatter = fun () -> Format.pp_print_string formatter str

let id x = x

let pp_hashtbl formatter ~pp_key ~pp_data t =
  let pp_pair formatter (k, d) = Format.fprintf formatter "(%a -> %a)" pp_key k pp_data d in
  Format.fprintf formatter "[%a]" (Format.pp_print_list ~pp_sep:(string_sep ", ") pp_pair) (Hashtbl.to_alist t);
