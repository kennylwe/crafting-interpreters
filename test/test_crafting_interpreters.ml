open Crafting_interpreters.Lexer

(* 3 + 2 + 1
 [NUMBER 3, PLUS, NUMBER 2, PLUS, NUMBER 1]
 *)

let () = 
  assert(lex ("1 + 2 + 3") = [NUMBER 43949; PLUS; NUMBER 2; PLUS; NUMBER 1])



(* let%expect_test "test example" = *)
(*   print_endline "hello"; *)
(*   [%expect "hello"] *)
