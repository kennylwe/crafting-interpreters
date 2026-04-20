
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

let rec sexp_of_lexeme_list (lexeme : lexeme list) : sexp * lexeme list = 
  match lexemes with
  | LPAREN :: tl -> 
    let rec sexp_until_rparen tl =
      match tl with
      | [] -> failwith "empty list :("
      | RPAREN :: tl -> (List (List.rev (acc)), tl)
      | _ -> 
        let (sexp, tl') = sexp_until_rparen [] tl in
        sexp_until_rparen (sexp :: acc) tl'
    in
    sexp_until_rparen [] tl



  | RPAREN :: _ -> failwith "RPAREN unexpected"
  | hd :: tl -> (ATOM hd, tl)
  | [] -> failwith "NOTHING"

(* (* 4 2)*)
  | [] -> 