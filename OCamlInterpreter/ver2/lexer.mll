let digit = ['0'-'9']
let space = ' ' | '\t' | '\r' | '\n'
let alpha = ['a'-'z' 'A'-'Z' '_' ] 
let ident = alpha (alpha | digit)* 

rule main = parse
| space+       { main lexbuf }
| "+"          { Parser.PLUS }
| "*"          { Parser.TIMES }
| "-"          { Parser.MINUS }
| "/"          { Parser.DIV }
| "&&"         { Parser.AND }
| "||"         { Parser.OR }
| "="          { Parser.EQ }
| "<"          { Parser.LT }
| "let"        { Parser.LET }
| "rec"        { Parser.REC }
|"in"         { Parser.IN }
| "and"        { Parser.LETAND }
| "if"         { Parser.IF }
| "then"       { Parser.THEN }
| "else"       { Parser.ELSE }
| "true"       { Parser.BOOL (true) }
| "false"      { Parser.BOOL (false) }
| "fun"        { Parser.FUN}
| "->"         { Parser.ARROW }
| "("          { Parser.LPAR }
| ")"          { Parser.RPAR }
| "exit"       { Parser.EXIT }
| ";;"         { Parser.SEMISEMI }
| digit+ as n  { Parser.INT (int_of_string n) }
| ident  as id { Parser.ID id }
| _            { (print_string ("Unknown Token: " ^ Lexing.lexeme lexbuf ^ "\n"); Parser.UnknownToken)}
