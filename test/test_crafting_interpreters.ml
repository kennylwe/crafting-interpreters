open Crafting_interpreters.Lexer

let () = assert (lex "1 + 2 + 3" = [ NUMBER 1; PLUS; NUMBER 2; PLUS; NUMBER 3 ])
(* assert(sexp_of_lexeme_list [NUMBER 1; PLUS; NUMBER 2] = List [Atom (NUMBER 1); Atom PLUS; Atom (NUMBER 2)]); *)
