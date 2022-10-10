(* Bianca Minusculli Pelegrini - 279598 *)

#use "types.ml"
#use "typeinfer.ml"
#use "eval.ml"
#use "auxFns.ml"

(* main *)
let bse (e:expr) (mem: memory) : unit =
  try
    let typ = typeinfer [] e in
    let (value, memory) = eval [] e mem in
      print_string ("VALUE: " ^ (vtos value) ^ ": " ^ (ttos typ) ^
                    "\nMEMORY: [" ^ (mtos memory) ^ "]\n ")
  with
    TypeError msg -> print_string ("[erro de tipo] " ^ msg)

  | NotImplemented msg -> print_string ("[bug - missing implementation of " ^ msg ^ "]")
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

let tstSkip = If(Binop(Gt, Num 8, Num 0), Skip, Skip)

(*  o programa abaixo retorna
        valor   = 7
        memória = [(l1, 4)]

    let x : int ref = new 3 in  -- x = end 1;
    let y : int = !x in  --  y = 3
        (x := !x + 1);   --
        y + !x
*)
let teste1 = Let("x", TyRef TyInt, New (Num 3),
                 Let("y", TyInt, Dref (Var "x"),
                     Seq(Asg(Var "x", Binop(Sum, Dref(Var "x"), Num 1)),
                         Binop(Sum, Var "y",  Dref (Var "x")))))


(*
    o programa abaixo retorna
    valor   = 1
    memória = [(l1, 1)]

  let x: int ref  = new 0 in
  let y: int ref  = x in
    x := 1;
    !y
*)
let teste2 = Let("x", TyRef TyInt, New (Num 0),
                 Let("y", TyRef TyInt, Var "x",
                     Seq(Asg(Var "x", Num 1),
                         Dref (Var "y"))))


(*  o programa abaixo retorna
      valor   = 3
      memória = [(l1, 2)]


    let counter : int ref = new 0  in
    let next_val : unit -> int =
        fn ():unit  =>
          counter := (!counter) + 1;
          !counter
    in  (next_val()  + next_val())
*)
let counter1 = Let("counter", TyRef TyInt, New (Num 0),
                   Let("next_val", TyFn(TyUnit, TyInt),
                       Fn("w", TyUnit,
                          Seq(Asg(Var "counter",Binop(Sum, Dref(Var "counter"), Num 1)),
                              Dref (Var "counter"))),
                       Binop(Sum, App (Var "next_val", Skip),
                             App (Var "next_val", Skip))))


(*   o programa abaixo retorna
     valor = 120
     memória = [(l2, 120), (l1,0) ]


    let fat (x:int) : int =

        let z : int ref = new x in
        let y : int ref = new 1 in

        while (!z > 0) (
           y :=  !y * !z;
           z :=  !z - 1;
        );
        ! y
    in
       fat 5



    SEM açucar sintático

    let fat : int-->int = fn x:int =>

        let z : int ref = new x in
        let y : int ref = new 1 in

        while (!z > 0) (
           y :=  !y * !z;
           z :=  !z - 1;
        );
        ! y
    in
       fat 5

*)
let whilefat = Whl(Binop(Gt, Dref (Var "z"), Num 0),
                   Seq( Asg(Var "y", Binop(Mult, Dref (Var "y"), Dref (Var "z"))),
                        Asg(Var "z", Binop(Sub, Dref (Var "z"), Num 1)))
                  )

let bodyfat = Let("z", TyRef TyInt, New (Var "x"),
                  Let("y", TyRef TyInt, New (Num 1), Seq (whilefat, Dref (Var "y"))))

let impfat = Let("fat", TyFn(TyInt,TyInt), Fn("x", TyInt, bodyfat), App(Var "fat", Num 5))
