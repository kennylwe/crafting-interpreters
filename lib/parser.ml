open Lexer

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
let parse (lexemes : lexeme list) : sexp = 
  let (sexpres, lexlist) = sexp_of_lexeme_list lexemes in
  if (lexlist <> []) then failwith "you added something bad loser" else sexpres

type binary_op = Add | Sub | Div | Mul | Lt | Gt


type expr =
  | Int of int
  | Bool of bool
  | If of expr * expr * expr
  | Binary of binary_op * expr * expr



let rec expr_of_sexp (s : sexp) : expr = 
  match s with
  | Atom (NUMBER i) -> Int i
  | Atom TRUE -> Bool true
  | Atom FALSE -> Bool false
  | List [Atom op; s1; s2] -> 
      let binary_op = 
        match op with 
        | STAR -> Mul
        | PLUS -> Add
        | MINUS -> Sub
        | DIVIDE -> Div
        | LT -> Lt
        | GT -> Gt
        | _ -> failwith "Invalid Operator"
      in
      Binary (binary_op, expr_of_sexp s1, expr_of_sexp s2)

  | List [Atom IF; s1; s2; s3] -> If (expr_of_sexp s1, expr_of_sexp s2, expr_of_sexp s3) 
  | _ -> failwith "Invalid expression"

  


(* TODO: Try converting sexps to ASTs! *)
(* TODO: Write a [string_of_sexp], which will need a [string_of_lexeme] *)
(* TODO: Write some simple example inputs and print outputs *)

