(*
REFERENCE

type shape = Circle | Square | Triangle

let f (b : shape) = 
  match b with
  | Circle -> "HI"
  | Square -> "HI"
  | Triangle -> "HI"

let f x y =
  let z = x + y in
  let w = x * y in
  z * w
*)



(** [Circle of radius], [Square of side], [Rectangle of l w] *)
type shape = Circle of int | Square of int | Rectangle of int * int

let area (shape : shape) = 
    match shape with 
    | Circle radius ->
        let radius = float_of_int radius in
        radius *. radius *. 3.14 
    | Square side -> float_of_int(side * side)
    | Rectangle (side1, side2) -> float_of_int(side1 * side2)


(*
type expr =
  | Int of int
  | Add of expr * expr
  | Mul of expr * expr

let x = Add (Mul (Add (Int 1, Int 2), Int 3), Int 4)

let rec simplify y = 
  match y with
  | Int x -> x
  | Add (x, y) -> simplify x + simplify y
  | Mul (x, y) -> simplify x * simplify y

(* simplify (Add (Int 5, Int 6)) *)

*)
type binary_op = Add | Sub | Div | Mul


type expr =
  | Int of int
  | Bool of bool
  | If of expr * expr * expr
  | Lt of expr * expr
  | Gt of expr * expr
  | Binary of binary_op * expr * expr

let x = Binary (Add, Binary (Mul, Binary (Add, Int 1, Int 2), Int 3), Int 4)

(* if 1 + 1 < 2 then 10 + 1 else 2 * 3 *)
let e = If (Lt (Int 1, Int 2), Add (Int 10, Int 1), Mul (Int 2, Int 3))

let get_int (e : expr) : int =
  match e with
  | Int i -> i
  | _ -> failwith "not an int"

let get_bool (e : expr) : bool = 
  match e with
  | Bool x -> x
  | _ -> failwith "not a boolean"

(* TODO: Finish this off *)
(* TODO: Tasks in book *)



let rec simplify (y : expr) : expr =
  (* let simpl_int e = get_int (simplify e) in *)
  match y with
  | Int x -> Int x
  | Bool x -> Bool x
  | Add (x, y) ->
    let x = get_int (simplify x) in
    let y = get_int (simplify y) in
    Int (x + y)
  | Mul (x, y) -> 
    let x = get_int (simplify x) in
    let y = get_int (simplify y) in
    Int (x * y)
  | If (x, y, z) ->
    let x = get_bool (simplify x) in
    (* [if] is a special form, "normal", doesn't compute unless n *)
    (* [+] is not a special form, "applicative", immediately applies *)
    if x then simplify y else simplify z
  | Lt (x, y) ->
    Bool (get_int (simplify x) < get_int (simplify y))
  | Gt (x, y) ->
    Bool (get_int (simplify x) > get_int (simplify y))

(* utop:
  [#use "filename.ml";;]
  Ctrl-D to quit
*)

let rec string_of_expr (e : expr) : string =
  match e with
  | Int i -> string_of_int i
  | Bool i -> string_of_bool i
  | Add (x, y) -> string_of_expr x ^ " + " ^ string_of_expr y
  | Mul (x, y) -> string_of_expr x ^ " * " ^ string_of_expr y
  | If (x, y, z) -> "if " ^ string_of_expr x ^ " then " ^ string_of_expr y ^ " else " ^ string_of_expr z
  | Lt (x, y) -> string_of_expr x ^ " < " ^ string_of_expr y
  | Gt (x, y) -> string_of_expr x ^ " > " ^ string_of_expr y


let () = print_endline (string_of_expr x)
