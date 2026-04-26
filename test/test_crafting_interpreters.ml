let () =
  assert (1 = 1);
  assert (2 = 2)

let%expect_test "test example" =
  print_endline "hello";
  [%expect "hello"]
