open Core
open Typed_latex
open Ast_print
open Util


let main () =
  let filename = "assets/tex/sample4.tex" in
  (* Pattern_defs.parse_patterns "assets/patterns/formatted-patterns.txt";  *)
  (* print_endline ""; *)
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
          ("\nFound "
          ^ string_of_int (List.length token_streams)
          ^ " token stream(s)"))
      <-<? tokenization;
      tokenization
      >->? List.iter ~f:(fun token_stream ->
               print_endline ("\n" ^ String.make 16 '=' ^ "\n");
               (fun stream ->
                 print_endline
                   ("Stream length: " ^ string_of_int (List.length stream)))
                 token_stream;
               List.iter token_stream ~f:(fun token ->
                   match token with
                   | Proof_lex.WordToken word ->
                       print_endline ("| WordToken: " ^ word)
                   | Proof_lex.MathToken _ -> print_endline "| MathToken");
               let matched_context =
                 Patterns.match_pattern pattern token_stream
               in
               print_endline
                 (if is_some matched_context then "\nMatched the pattern"
                  else "\nDid not match the pattern");
               let matches = Pair.second |<<? matched_context in
               print_endline << Patterns.MatchContainer.tree_format <-<? matches)

let () = old_main ()
