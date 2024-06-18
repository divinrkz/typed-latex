open Core
open Typed_latex
(* open Util *)

(* TODO: add basic expansion step to expand user-defined macros *)

(* TODO: write a test suite *)
  (* let result = parse_latex "\\begin{document}$1+2$\\end{document}" in *)
  (* let result = parse_latex "$\\forall x \\in \\mathbb{R}, \\exists y \\: x = 2 \\land (y = 2 \\lor z = 3)$" in *)
  (* let result = parse_latex "$f(x, y) = \\frac{e^{-x}}{1 - e^y}$" in *)
  (* let result = parse_latex "$x \\in T \\land y \\notin U$" in *)
  (* let result = parse_latex "$\\{ x^n | n \\in \\mathbb{R} \\}$" in *)
  (* let result = parse_latex "$f(x) = x^2 = x * x \\leq 0$" in *)
  (* let result = parse_latex "$\\forall \\epsilon > 0, \\exists \\delta < \\epsilon s.t. \\forall x, 0 < |x| < \\delta \\implies |f(x)| < \\epsilon$" in *)
  (* let result = parse_latex "$x \\in \\mathbb{R}$\n$y = \\{x, z\\}$" in *)
  (* let result = parse_latex "$P(x, y) = x - N^y$\n$P(1, 2)$\n$N = \\frac{1}{2}$" in *)

let main () =
  let filename = "tex/sample3.tex" in
  let result = try User.parse_latex_file filename with
  | User.Error _ as e -> fprintf stderr "%s\n" (User.error_message e); exit (-1)
  in
  match result with
  | None -> fprintf stderr "Unable to parse. Exiting...\n"; exit (-1)
  | Some ast -> (
    (* Format.printf "Parsed latex: %a" Ast.Latex.pp ast; *)
    (* try User.type_check ast with *)
    (* | User.Error _ as e -> fprintf stderr "%s\n" (User.error_message e); exit (-1) *)
    (* let pattern = User.Sequence [Word "Hello"; Variable 0] in *)
    match User.match_with ast User.def with
    | Some mappings -> Format.printf "Success: %a\n" (Util.pp_hashtbl ~pp_key:Format.pp_print_int ~pp_data:Ast.Math.pp) mappings
    | None -> Format.printf "Fail\n"
  )

let () = main ();;
