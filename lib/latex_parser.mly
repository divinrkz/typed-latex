%{
  (* fix a weird menhir issue: https://github.com/ocaml/dune/issues/2450 *)
  module Typed_latex = struct end
%}

%token <Lexing.position> LINE_BREAK
%token <Lexing.position> WHITESPACE
%token <Lexing.position> LEFT_CURLY
%token <Lexing.position> LEFT_BRACKET
%token <Lexing.position> LEFT_PAREN
%token <Lexing.position> RIGHT_CURLY
%token <Lexing.position> RIGHT_BRACKET
%token <Lexing.position> RIGHT_PAREN
%token <Lexing.position> COMMA
%token <Lexing.position> PERIOD
%token <Lexing.position> AMPERSAND
%token <Lexing.position> PIPE
%token <Lexing.position> EQ
%token <Lexing.position * string> WORD
%token <Lexing.position * string> COMMAND
%token <Lexing.position> EOF

%token <Lexing.position * string> BEGIN
%token <Lexing.position * string> END
%token <Lexing.position * string> MATHMODE
%token <Lexing.position * string> MULTILINE

%start <Ast.Latex.t option> start

%%

start:
| node = latex; EOF { Some node }
| EOF { None }

latex:
| all = content*; { {Ast.Node.pos = Lexing.dummy_pos; value = Ast.Latex.Latex all} }

content:
| env = environment { env }
| text = word { text } 
| math = MATHMODE { {Ast.Node.pos = fst math; value = Ast.Latex.Mathmode (snd math)} } 
| math = MULTILINE { {Ast.Node.pos = fst math; value = Ast.Latex.Multiline (snd math)} } 
| command = command { command } 
| LEFT_CURLY; latex = latex; RIGHT_CURLY { latex } 

(* TODO: convert into variant for more semantic information *)
word:
| word_data = WORD; { {Ast.Node.pos = fst word_data; value = Ast.Latex.Word (snd word_data)} }
| data = LINE_BREAK; LINE_BREAK* { {Ast.Node.pos = data; value = Ast.Latex.Newline} }
| data = WHITESPACE; WHITESPACE* { {Ast.Node.pos = data; value = Ast.Latex.Whitespace} }
| data = COMMA { {Ast.Node.pos = data; value = Ast.Latex.Word ","} }
| data = PERIOD { {Ast.Node.pos = data; value = Ast.Latex.Word "."} }
| data = PIPE { {Ast.Node.pos = data; value = Ast.Latex.Word "|"} }
| data = LEFT_PAREN { {Ast.Node.pos = data; value = Ast.Latex.Word "("} }
| data = RIGHT_PAREN { {Ast.Node.pos = data; value = Ast.Latex.Word ")"} }
| data = LEFT_BRACKET { {Ast.Node.pos = data; value = Ast.Latex.Word "["} }
| data = RIGHT_BRACKET { {Ast.Node.pos = data; value = Ast.Latex.Word "]"} }
| data = EQ { {Ast.Node.pos = data; value = Ast.Latex.Word "="} }

(* a dumb hack to prevent the parser from failing when "]" is located inside of bracketed command arguments *)
(* otherwise, in something like \command[asdf], the parser would recognize the "]" not as the end of the grouping but as a word instead *)
latex_no_rbracket:
| all = content1*; { {Ast.Node.pos = Lexing.dummy_pos; value = Ast.Latex.Latex all} }
content1:
| env = environment { env }
 (* below leads to a shift/reduce conflict, but menhir seems to work fine anyway *)
| text = word_no_rbracket { text } 
| math = MATHMODE { {Ast.Node.pos = fst math; value = Ast.Latex.Mathmode (snd math)} } 
| math = MULTILINE { {Ast.Node.pos = fst math; value = Ast.Latex.Multiline (snd math)} } 
| command = command { command } 
word_no_rbracket:
| word_data = WORD; { {Ast.Node.pos = fst word_data; value = Ast.Latex.Word (snd word_data)} }
| data = LINE_BREAK { {Ast.Node.pos = data; value = Ast.Latex.Word "\n"} }
| data = WHITESPACE { {Ast.Node.pos = data; value = Ast.Latex.Word " "} }
| data = COMMA { {Ast.Node.pos = data; value = Ast.Latex.Word ","} }
| data = PIPE { {Ast.Node.pos = data; value = Ast.Latex.Word "|"} }
| data = LEFT_PAREN { {Ast.Node.pos = data; value = Ast.Latex.Word "("} }
| data = RIGHT_PAREN { {Ast.Node.pos = data; value = Ast.Latex.Word ")"} }
| data = LEFT_BRACKET { {Ast.Node.pos = data; value = Ast.Latex.Word "["} }
| data = EQ { {Ast.Node.pos = data; value = Ast.Latex.Word "="} }

command:
| command = COMMAND; args = command_args; { {Ast.Node.pos = fst command; value = Ast.Latex.Command (snd command, args) } } 

command_args:
| LEFT_BRACKET; contents = latex_no_rbracket; RIGHT_BRACKET; rest = curly_args* { contents :: rest }
| rest = curly_args* { rest }

curly_args:
| LEFT_CURLY; contents = latex; RIGHT_CURLY { contents }

environment:
| name1 = BEGIN; args = command_args; body = content+; END; { {Ast.Node.pos = fst name1; value = Ast.Latex.Environment (snd name1, args, body)} }

