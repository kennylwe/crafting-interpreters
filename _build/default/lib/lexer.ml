(* type lexemes = PLUS | MINUS | LPAREN | RPAREN *)
(* "a + b * (c + d)" (raw string) *)
(* ["a", "+", "b", "*", "(", "c", "+", "d", "1", ".", "2"] (tokens) *)
(* [ID "a", PLUS, ID "b", STAR, LPAREN, ID "c", PLUS, ID "d", FLOAT 1.2] (lexemes) *)
(* Add (Var "a", Mul (Var b, Add (Var c, Var d))) (parser) *)

(* TODO: Add more lexemes to support the full [expr] type *)
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

(*
  1. Parse through string
  2. If white space ignore and go to next
  3. else add to list
  4. check through lexeme for corresponding type
*)
(** Check if [c] is alphabetical *)
let is_alpha (c : char) : bool = 
  let c = Char.lowercase_ascii c in
  Char.code c >= Char.code 'a' && Char.code c <= Char.code 'z'

let is_numeric (c : char) : bool = 
  Char.code c >= Char.code '0' && Char.code c <= Char.code '9'

let tail (s: string) : string = String.sub s 1 (String.length s)

(* "then else 12 32" -> ("then", "else 12 32") *)


let rec read_while (pred: char -> bool) (s : string) : string * string =
  if pred s.[0] then
    let (left, right) = read_while (pred)(tail s) in
    (String.of_seq (Seq.cons s.[0] (String.to_seq left)), right) else
      ("", s)

let read_until_non_alpha = read_while is_alpha
let read_until_non_numeric = read_while is_alpha

let rec lex (e : string) : lexeme list =
  match e.[0] with
  | '*' -> STAR :: lex (String.sub e 1 (String.length e))
  (* TODO: Factor out   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ *)
  | '-' -> MINUS :: lex (String.sub e 1 (String.length e))
  | '/' -> DIVIDE :: lex (String.sub e 1 (String.length e))
  | '+' -> PLUS :: lex (String.sub e 1 (String.length e))
  | '(' -> LPAREN :: lex (String.sub e 1 (String.length e))
  | ')' -> RPAREN :: lex (String.sub e 1 (String.length e))
  
  (* TODO: Do the rest of them: *)
  | c when is_alpha c ->
      let left, right = read_until_non_alpha e in
      let head =
        match left with
        | "if" -> IF
        | "then" -> THEN
        | "True" -> TRUE
        | "False" -> FALSE
        | "else" -> ELSE
        (* TODO: Do the rest of them: *)
        | _ -> ID left
      in
      head :: lex right
  | c when is_numeric c ->
      let left, right = read_until_non_numeric e in
      NUMBER (int_of_string left) :: lex right
  | _ -> failwith "Unexpected token"