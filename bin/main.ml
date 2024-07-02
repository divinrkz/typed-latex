open Core
open Typed_latex
open Ast_print
open Patterns
open Util

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
  let filename = "tex/sample4.tex" in
  let seq = Pattern_defs.parse_patterns "pattern.txt" in 
  (* let matches = Util.regex_matcher "let $" "[a-zA-Z]+" in  *)
  print_endline "";
  let parsed_latex =
    try User.parse_latex_file filename
    with User.Error _ as e ->
      fprintf stderr "%s\n" (User.error_message e);
      exit (-1)
  in
  match parsed_latex with
  | None ->
      fprintf stderr "Unable to parse. Exiting...\n";
      exit (-1)
  | Some _ ->
      print_endline "Parsed latex.";
      let document_ast = User.unwrap_to_document =<<? parsed_latex in
      print_endline << latex_tree_format <-<? document_ast;
      let pattern = Pattern_defs.def1 in
      let tokenization = Proof_lex.tokenize |<<? document_ast in
      (fun token_streams ->
        print_endline
          ("Found "
          ^ string_of_int (List.length token_streams)
          ^ " token stream(s)"))
      <-<? tokenization;
      let first_token_stream = List.hd =<<? tokenization in
      (fun stream ->
        print_endline ("Stream length: " ^ string_of_int (List.length stream)))
      <-<? first_token_stream;
      List.iter ~f:(fun token ->
          match token with
          | Proof_lex.WordToken word -> print_endline ("| WordToken: " ^ word)
          | Proof_lex.MathToken _ -> print_endline "| MathToken")
      <-<? first_token_stream;
      let matched_context =
        Patterns.match_pattern pattern =<<? first_token_stream
      in
      print_endline
        (if is_some matched_context then "Matched the pattern"
         else "Did not match the pattern");
      let matches = Pair.second |<<? matched_context in
      print_endline << Patterns.MatchContainer.tree_format <-<? matches

let () = main ()
