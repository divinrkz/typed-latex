open Core
open Typed_latex
open Latex_deserializer
open Util
open String_tree
open Proof_lex
open Pattern_extractor
open Patterns

module Json = Yojson.Basic

let main () =
  let filename = "assets/json/parsed-latex.json" in

  
  let extractor = PatternExtractor.constr in 
    PatternExtractor.extract_patterns extractor "assets/patterns/patterns.txt";
  let extracted_patterns = PatternExtractor.get_patterns extractor in 
    extracted_patterns >->: (fun pattern -> 
      print_string ("\nLine [" ^ (string_of_int (PatternDef.get_id pattern)) ^ "]: ");
      print_endline ("\"" ^ PatternDef.get_line pattern ^ "\"");
      print_endline (show_pattern (PatternDef.get_pattern pattern))
    ); 
  
  let json = Json.from_file filename in
  let parsed_latex = RawLatex.deserialize_from_json json in

  let opt_latex = Result.ok parsed_latex in
  let document_ast = Latex_aux.unwrap_to_document =<<? opt_latex in
  print_endline << RawLatex.tree_format <-<? document_ast;
  let pattern = Patterns.Word "TEST" in
  let tokenization = Proof_lex.tokenize |<<? document_ast in
  
  
  (fun token_streams ->
    print_endline
      ("\nFound "
      ^ string_of_int (List.length token_streams)
      ^ " token stream(s)")
      )
  <-<? tokenization;
  tokenization
  >->? List.iter ~f:(fun token_stream ->
           (* print_endline ("\n" ^ String.make 16 '='); *)
           (* print_endline
             (tree_format "| "
                (Branch
                   ( Some
                       ("Stream length: "
                       ^ string_of_int (List.length token_stream)),
                     token_stream >>|: ProofToken.to_string_tree )));
           let matched_context = Patterns.match_pattern pattern token_stream in
           (* print_endline
             (if is_some matched_context then "\nMatched the pattern"
              else "\nDid not match the pattern");
           let matches = Triple.second |<<? matched_context in
           print_endline << Patterns.MatchContainer.tree_format <-<? matches)

let () = main ()
