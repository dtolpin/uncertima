(* $Id: plexer.mll 420 2009-05-23 17:26:01Z tolpin $ *)

(* lexer for problem definition *)

{
open Lexing
open Pparser

let keywords =
  [ "problem", PROBLEM;
    "space", SPACE;
    "observe", OBSERVE;
    "find", FIND;
    "max", MAX;
    "min", MIN;
    "model", MODEL;
    "indep", INDEP;
    "lattice", LATTICE;
    "bif", BIF;
    "gdl", GDL;
    "Inf", INFINITY ] 
;;

let kwd_or_ident word =
  try List.assoc word keywords 
  with _ -> IDENT word
;;

(* strings are rare *)
let unescaped s =
  match Stream.next (Genlex.make_lexer [] (Stream.of_string s)) with
    Genlex.String s -> s
  | _ -> failwith "string token is not a Genlex string"
;;

let newline lexbuf = 
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- { pos with
                         pos_lnum = pos.pos_lnum + 1;
                         pos_bol = pos.pos_cnum;
                       }

}

let comment = "#"
let newline = ('\010' | '\013' | "\013\010" )
let blank = [' ' '\009' '\012']
let int = ['0'-'9']+
let float = ['0'-'9']+ ('.' ['0'-'9']+ )?(['e' 'E'] ['+' '-']? ['0'-'9']+)?
let alpha = ['A'-'Z' 'a'-'z']
let digit = ['0'-'9']
let ident = alpha (alpha|digit|'_')*
let string = '"' ([^ '"']|"\\\"")* '"'

rule token = parse
  newline { newline lexbuf; token lexbuf }
| blank+  { token lexbuf }
| comment { comment lexbuf }
| ident { kwd_or_ident (lexeme lexbuf) }
| string { STRING (unescaped (lexeme lexbuf)) }
| int { INT (int_of_string(lexeme lexbuf)) }
| float { FLOAT (float_of_string(lexeme lexbuf)) }
| '('  { LPAR }     | ')'  { RPAR }
| '+'  { PLUS }     | '-'  { MINUS }
| '*'  { STAR }     | '/'  { SLASH }
| '%'  { PERC }     | '^'  { POWER }
| '{'  { LCUR }     | '}'  { RCUR }
| '['  { LSQU }     | ']'  { RSQU }
| ','  { COMMA }    | ';'  { SEMIC }
| '<'  { LT }       | '>'  { GT }
| "<=" { LE }       | ">=" { GE }
| '='  { EQ }       | "/=" { NE }
| "=>" { RARR }     | '@'  { AT }
| ".." { DOTS }   
| eof { EOF }
| _ { raise (Pdefinition.ParseError (Pdefinition.Illegal_char (lexeme_char lexbuf 0),
                                     lexbuf.lex_start_p)) }
and comment = parse
  newline { newline lexbuf; token lexbuf }
| eof { EOF }
| _ { comment lexbuf }



