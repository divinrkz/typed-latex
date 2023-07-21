open Core
open Lexing
open Typed_latex

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error parser lexer str =
  let lexbuf = (from_string str) in
  try parser lexer lexbuf with
  | Lexer.LexError msg ->
    fprintf stderr "%a: %s\n" print_position lexbuf msg;
    None
  | Parser.Error ->
    fprintf stderr "%a: parse error\n" print_position lexbuf;
    exit (-1)

(* TODO: add basic expansion step to expand user-defined macros *)
let parse_latex = parse_with_error Parser.start Lexer.token

let parse_math = parse_with_error Parser.math_mode Lexer.math_token

(* TODO: write a test suite *)

let main () =
  (* let result = parse_latex "\\begin{document}$1+2$\\end{document}" in *)
  (* let result = parse_latex "$\\forall x \\in \\mathbb{R}, \\exists y \\: x = 2 \\land (y = 2 \\lor z = 3)$" in *)
  (* let result = parse_latex "$f(x, y) = \\frac{e^{-x}}{1 - e^y}$" in *)
  (* let result = parse_latex "$x \\in T \\land y \\notin U$" in *)
  (* let result = parse_latex "$\\{ x^n | n \\in \\mathbb{R} \\}$" in *)
  (* let result = parse_latex "$f(x) = x^2 = x * x \\leq 0$" in *)
  (* let result = parse_latex "$\\forall \\epsilon > 0, \\exists \\delta < \\epsilon s.t. \\forall x, 0 < |x| < \\delta \\implies |f(x)| < \\epsilon$" in *)
  (* let result = parse_latex "$x \\in \\mathbb{R}$\n$y = \\{x, z\\}$" in *)
  let result = parse_latex "$P(x, y) = x + y * N$\n$P(1, 2)$\n$N = 3$" in
  match result with
  | Some ast -> (
    (* Format.printf "%a\n" (Format.pp_print_list Ast.Latex.pp) ast; *)
    let open List in
    let math_nodes = ref [] in
    let _ = ast >>| fun x -> match x with
      | {pos = _; value = Ast.Latex.Mathmode str} -> (
        Format.printf "Parsing math: \"%s\"...\n" str;
        let result = parse_math str in
        match result with
          | Some ast -> (
            Format.printf "%a\n" Ast.Math.pp ast;
            math_nodes := ast :: !math_nodes;
          )
          | None -> printf "No math\n")
      | _ -> ()
    in
    Ast.Math.type_check !math_nodes
  )
  | None -> print_endline "None"

let () = main ();;

(* let () = Typing.test ();; *)
