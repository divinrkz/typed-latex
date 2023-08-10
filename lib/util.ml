open Core

let string_sep str formatter = fun () -> Format.pp_print_string formatter str

let id x = x

let pp_hashtbl formatter ~pp_key ~pp_data t =
  Format.fprintf formatter "[";
  Hashtbl.iteri t ~f:(fun ~key ~data ->
    Format.fprintf formatter "(%a -> %a), " pp_key key pp_data data
  );
  Format.fprintf formatter "]\n";
