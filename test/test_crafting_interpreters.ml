open Crafting_interpreters.Lexer
open Crafting_interpreters.Parser

let () = assert (lex "1 + 2 + 3" = [ NUMBER 1; PLUS; NUMBER 2; PLUS; NUMBER 3 ])
let () = assert (parse (lex "(1 + 2)") = List [Atom (NUMBER 1); Atom PLUS; Atom (NUMBER 2)])
(* assert(sexp_of_lexeme_list [NUMBER 1; PLUS; NUMBER 2] = List [Atom (NUMBER 1); Atom PLUS; Atom (NUMBER 2)]); *)
