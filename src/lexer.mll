{
let reserverWords = [
  ("zero", Parser.ZERO);
  ("succ", Parser.SUCC);
]
;;

let proj_of_string str =
  Scanf.sscanf str "@%d/%d" (fun x y -> (x, y));;

let maybe_assoc default key alist =
  try 
    List.assoc key alist
  with
    Not_found -> default
;;
}

rule main = parse
  [' ' '\009' '\012' '\n']+ { main lexbuf }
| ['0'-'9']+ 
  { Parser.INTV (int_of_string (Lexing.lexeme lexbuf)) }
| "("  { Parser.LPAREN }
| ")"  { Parser.RPAREN }
| "["  { Parser.LBRAKET }
| "]"  { Parser.RBRAKET }
| "->" { Parser.RARROW }
| "."  { Parser.DOT }
| ";;" { Parser.SEMISEMI }
| ","  { Parser.COMMA }
| "@" ['0'-'9']+ "/" ['0'-'9']+
  { Parser.PROJECTOR (proj_of_string (Lexing.lexeme lexbuf)) }
| ['a'-'z']['a'-'z' '0'-'9' '_']*
  { let id = Lexing.lexeme lexbuf in maybe_assoc (Parser.ID id) id reserverWords }
