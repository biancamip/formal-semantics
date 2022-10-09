(* Bianca Minusculli Pelegrini - 279598 *)

#use "types.ml"
#use "typeinfer.ml"
#use "eval.ml"

#use "auxFns.ml"

(* main *)
let bse (e:expr) : unit =
  try
    let typ = typeinfer [] e in
    let value = eval [] e
    in  print_string ((vtos value) ^ " : " ^ (ttos typ))
  with
    TypeError msg -> print_string ("[erro de tipo] " ^ msg)

  | NotImplemented  -> print_string "[something was not implemented yet]"
  | BugTypeInfer    -> print_string "[bug - typeinfer] "
  | BugParser       -> print_string "[bug - parser] "

(* +++++++++++++++++++++++++++++++++++++++*)
(*                TESTES                  *)
(*++++++++++++++++++++++++++++++++++++++++*)

(*
  let x:int = 2
    in let foo: int --> int = fn y:int => x + y
      in let x: int = 5
        in foo 10

  do tipo int, tst avalia para 12
*)

let e'' = Let("x", TyInt, Num 5, App(Var "foo", Num 10))
let e'  = Let("foo", TyFn(TyInt,TyInt), Fn("y", TyInt, Binop(Sum, Var "x", Var "y")), e'')

let tst = Let("x", TyInt, Num(2), e')

(*
  let x:int = 2
    in let foo: int --> int = fn y:int => x + y
      in let x: int = 5
        in foo

  do tipo int --> int, tst2 avalia para uma função
*)

let e2 = Let("x", TyInt, Num 5, Var "foo")
let e1  = Let("foo", TyFn(TyInt,TyInt), Fn("y", TyInt, Binop(Sum, Var "x", Var "y")), e2)

let tst2 = Let("x", TyInt, Num(2), e1)

