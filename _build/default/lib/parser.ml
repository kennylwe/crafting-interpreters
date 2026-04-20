
type lexeme =
  | ID of string
  | NUMBER of int
  | STAR
  | PLUS
  | MINUS
  | DIVIDE
  | IF
  | THEN
  | ELSE
  | LT
  | GT
  | TRUE
  | FALSE
  | LPAREN
  | RPAREN

type sexp = Atom of lexeme | List of sexp list

let rec sexp_of_lexeme_list lexemes = 
  match lexemes with
  | STAR :: tl -> INVALID
  | [] -> 