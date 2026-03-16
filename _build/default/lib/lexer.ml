
type lexeme = ID of string | NUMBER of int | STAR | PLUS | MINUS | IF | THEN | ELSE | LESS | GREATER | TRUE | FALSE | LPAREN | RPAREN
(*
  1. Parse through string
  2. If white space ignore and go to next
  3. else add to list
  4. check through lexeme for corresponding type
*)

