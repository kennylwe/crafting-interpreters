
type lexeme = ID of string | NUMBER of int | STAR | PLUS | MINUS | IF | THEN | ELSE | LESS | GREATER | TRUE | FALSE | LPAREN | RPAREN
(*
  1. Parse through string
  2. If white space ignore and go to next
  3. else add to list
  4. check through lexeme for corresponding type
*)

let rec lex (e : string) : lexeme list = 
  match e.[0] with
  | '*' -> STAR :: lex (String.sub e 1 ( String.length e ))
  | '+' -> PLUS :: lex (String.sub e 1 ( String.length e ))
  | 
  | _ -> lex String.sub(e, 0, 1), parse
