type lexeme =
  | ID of string
  | NUMBER of int
  | STAR
  | PLUS
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

let rec sexp_of_lexeme_list (lexemes : lexeme list) : sexp * lexeme list =
  match lexemes with
  | [] -> failwith "unexpected empty list"
  | RPAREN :: _ -> failwith "RPAREN unexpected"
  (*  ( ( + 1 2 3) 2 )  *)
  | LPAREN :: tl ->
      let rec sexp_until_rparen acc tl =
        match tl with
        | [] -> failwith "unexpected empty list"
        | RPAREN :: tl -> (List (List.rev acc), tl)
        | _ ->
            let (sexp, tl') = sexp_of_lexeme_list tl in
            sexp_until_rparen (sexp :: acc) tl'
      in
      sexp_until_rparen [] tl

      (* let (sexp, tl') = sexp_of_lexeme_list tl in *)
      (* let (sexp', tl'') = sexp_of_lexeme_list tl' in *)
      (* (List [sexp; sexp'], tl'' (without the RPAREN)) *)

  | hd :: tl -> (Atom hd, tl)

(* TODO: parse function should call [sexp_of_lexeme_list], but just
 check that the remaining [lexeme list] is empty and return the [sexp] *)
let parse _ = failwith "TODO"

(* TODO: Try converting sexps to ASTs! *)
(* TODO: Write a [string_of_sexp], which will need a [string_of_lexeme] *)
(* TODO: Write some simple example inputs and print outputs *)

