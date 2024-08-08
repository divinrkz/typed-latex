open Core
open Typed_latex
open Latex_deserializer
open Util
open String_tree
open Proof_lex
module Json = Yojson.Basic

let main () =
  let filename = "assets/json/parsed-latex.json" in
  let json = Json.from_file filename in
  let parsed_latex = RawLatex.deserialize_from_json json in
  print_endline
    (Result.tell
       (RawLatex.tree_format
       |<<! (( ^ ) "PARSE ERROR: " << RawLatex.string_of_parse_error
           |<<!! parsed_latex)));
  let opt_latex = Result.ok parsed_latex in
  let document_ast = Latex_aux.unwrap_to_document =<<? opt_latex in
  print_endline << RawLatex.tree_format <-<? document_ast;
  let pattern = Pattern_defs.def in
  let tokenization = Proof_lex.tokenize |<<? document_ast in
  (fun token_streams ->
    print_endline
      ("\nFound "
      ^ string_of_int (List.length token_streams)
      ^ " token stream(s)"))
  <-<? tokenization;
  tokenization
  >->? List.iter ~f:(fun token_stream ->
           print_endline ("\n" ^ String.make 16 '=');
           print_endline
             (tree_format "| "
                (Branch
                   ( Some
                       ("Stream length: "
                       ^ string_of_int (List.length token_stream)),
                     token_stream >>|: ProofToken.to_string_tree )));
           let matched_context = Patterns.match_pattern pattern token_stream in
           print_endline
             (if is_some matched_context then "\nMatched the pattern"
              else "\nDid not match the pattern");
           let matches = Triple.second |<<? matched_context in
           print_endline << Patterns.MatchContainer.tree_format <-<? matches)

let () = main ()
