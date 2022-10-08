(* Bianca Minusculli Pelegrini *)


(*++++++++++++++++++++++++++++++++++++++*)
(*  Interpretador para L1               *)
(*++++++++++++++++++++++++++++++++++++++*)

(* L1 lang types *)

type tipo =
    TyInt
  | TyBool
  | TyFn of tipo * tipo
  | TyPair of tipo * tipo

(* L1 lang expressions *)

type ident = string

type op = Sum | Sub | Mult | Eq | Gt | Lt | Geq | Leq

type expr =
  | Num of int
  | Var of ident
  | Bcte of bool
  | Binop of op * expr * expr
  | Pair of expr * expr
  | Fst of expr
  | Snd of expr
  | If of expr * expr * expr
  | Fn of ident * tipo * expr
  | App of expr * expr
  | Let of ident * tipo * expr * expr
  | LetRec of ident * tipo * expr  * expr

  (* | Question of expr * expr * expr
  | Dollar of expr * expr *)

type tenv = (ident * tipo) list

type valor =
    VNum of int
  | VBool of bool
  | VPair of valor * valor
  | VClos  of ident * expr * renv
  | VRclos of ident * ident * expr * renv
and
  renv = (ident * valor) list


let rec lookup a k =
  match a with
    [] -> None
  | (y,i) :: tl -> if (y=k) then Some i else lookup tl k

let update a k i = (k,i) :: a

(**+++++++++++++++++++++++++++++++++++++++++*)
(*               TYPE INFER                 *)
(*++++++++++++++++++++++++++++++++++++++++++*)

(* these should never be raised  *)
exception BugParser
exception BugTypeInfer

(* L1 user should be warned whenever this exception gets raised *)
exception TypeError of string

let rec typeinfer (tenv:tenv) (e:expr) : tipo =
  match e with

    (* TInt  *)
  | Num _ -> TyInt

    (* TVar *)
  | Var x ->
      (match lookup tenv x with
        Some t -> t
       | None -> raise (TypeError ("variavel nao declarada:" ^ x)))

    (* TBool *)
  | Bcte _ -> TyBool

    (* Top+ e outros operadores binários *)
  | Binop(operation,e1,e2) ->
      let type1 = typeinfer tenv e1 in
      let type2 = typeinfer tenv e2 in

      (match (type1, type2) with
        (TyInt, TyInt) ->
          (match operation with
            Sum | Sub | Mult         -> TyInt
            | Eq | Lt | Gt | Geq | Leq -> TyBool)
        | _ -> raise (TypeError "pelo menos um dos operandos de operador binário não é do tipo int"))

    (* TPair *)
  | Pair(e1, e2) -> TyPair(typeinfer tenv e1, typeinfer tenv e2)

    (* TFst *)
  | Fst e1 ->
      (match typeinfer tenv e1 with
        TyPair(t1,_) -> t1
       | _ -> raise (TypeError "fst espera tipo par ordenado"))

    (* TSnd *)
  | Snd e1 ->
      (match typeinfer tenv e1 with
        TyPair(_,t2) -> t2
       | _ -> raise (TypeError "snd espera tipo par ordenado"))

    (* TIf *)
  | If(e1,e2,e3) ->
      (match typeinfer tenv e1 with
        TyBool ->
          let secondType = typeinfer tenv e2 in
          let thirdType = typeinfer tenv e3 in
            if secondType = thirdType
              then secondType
              else raise (TypeError "then/else de tipos diferentes")
       | _ -> raise (TypeError "condição de IF não é do tipo bool"))

    (* TFn *)
  | Fn(x,t,e1) ->
      let t1 = typeinfer (update tenv x t) e1
      in TyFn(t,t1)

    (* TApp *)
  | App(e1,e2) ->
      (match typeinfer tenv e1 with
        TyFn(t, t') ->
          if (typeinfer tenv e2) = t
            then t'
            else raise (TypeError "tipo argumento errado para app" )
       | _ -> raise (TypeError "tipo função era esperado para App"))

    (* TLet *)
  | Let(x,t,e1,e2) ->
      if (typeinfer tenv e1) = t
        then typeinfer (update tenv x t) e2
        else raise (TypeError "expr nao é do tipo declarado em Let" )

    (* TLetRec *)
  | LetRec(f,(TyFn(t1,t2) as tf), Fn(x,tx,e1), e2) ->
      let tenv_com_tf = update tenv f tf in
      let tenv_com_tf_tx = update tenv_com_tf x tx in
      if (typeinfer tenv_com_tf_tx e1) = t2
        then typeinfer tenv_com_tf e2
        else raise (TypeError "tipo da funcao diferente do declarado")

  | LetRec _ -> raise BugParser
(*
  (* TQuestion  *)
  | Question(e1, e2, e3) ->
      (match typeinfer tenv e1 with
         TyInt ->
           let t2 = typeinfer tenv e2 in
           let t3 = typeinfer tenv e3
           in if (t2 = t3 && t2 = TyBool) then TyBool
           else raise (TypeError "tipos inválidos para os últimos operandos de ?")
       | _ -> raise (TypeError "primeiro operando de ? não é do tipo int"))

  (* TDollar  *)
  | Dollar(e1, e2) ->
      (match typeinfer tenv e1 with
         TyBool ->
           (match typeinfer tenv e2 with
              TyInt -> TyInt
            | _ -> raise (TypeError "segundo operando de $ não é do tipo int"))
       | _ -> raise (TypeError "primeiro operando de $ não é do tipo bool")) *)

(**+++++++++++++++++++++++++++++++++++++++++*)
(*                  EVAL                    *)
(*++++++++++++++++++++++++++++++++++++++++++*)

let rec compute (operation: op) (v1: valor) (v2: valor) : valor =
  match (operation, v1, v2) with

  (* arithmetic *)
    (Sum,  VNum(n1), VNum(n2)) -> VNum (n1 + n2)
  | (Sub,  VNum(n1), VNum(n2)) -> VNum (n1 - n2)
  | (Mult, VNum(n1), VNum(n2)) -> VNum (n1 * n2)

  (* relational *)
  | (Eq,  VNum(n1), VNum(n2)) -> VBool(n1 = n2)
  | (Gt,  VNum(n1), VNum(n2)) -> VBool(n1 > n2)
  | (Lt,  VNum(n1), VNum(n2)) -> VBool(n1 < n2)
  | (Geq, VNum(n1), VNum(n2)) -> VBool(n1 >= n2)
  | (Leq, VNum(n1), VNum(n2)) -> VBool(n1 <= n2)

  | _ -> raise BugTypeInfer


let rec eval (renv:renv) (e:expr) : valor =
  match e with

    Num n -> VNum n
  | Bcte b -> VBool b

  | Var x ->
      (match lookup renv x with
        Some v -> v
       | None -> raise BugTypeInfer)

  | Binop(operand,e1,e2) ->
      let v1 = eval renv e1 in
      let v2 = eval renv e2 in
      compute operand v1 v2


  | Pair(e1,e2) ->
      let v1 = eval renv e1 in
      let v2 = eval renv e2
      in VPair(v1,v2)

  | Fst e ->
      (match eval renv e with
        VPair(v1,_) -> v1
       | _ -> raise BugTypeInfer)

  | Snd e ->
      (match eval renv e with
        VPair(_,v2) -> v2
       | _ -> raise BugTypeInfer)


  | If(e1,e2,e3) ->
      (match eval renv e1 with
        VBool true   -> eval renv e2
       | VBool false -> eval renv e3
       | _ -> raise BugTypeInfer)

  | Fn (x,_,e1) ->  VClos(x,e1,renv)

  | App(e1,e2) ->
      let v1 = eval renv e1 in
      let v2 = eval renv e2 in
      (match v1 with
        VClos(x,ebdy,renv') ->
          let renv'' = update renv' x v2
          in eval renv'' ebdy

       | VRclos(f,x,ebdy,renv') ->
          let renv''  = update renv' x v2 in
          let renv''' = update renv'' f v1
          in eval renv''' ebdy

       | _ -> raise BugTypeInfer)

  | Let(x,_,e1,e2) ->
      let v1 = eval renv e1
      in eval (update renv x v1) e2

  | LetRec(f, TyFn(t1,t2), Fn(x,tx,e1), e2) when t1 = tx ->
      let renv'= update renv f (VRclos(f,x,e1,renv))
      in eval renv' e2
  | LetRec _ -> raise BugParser

(*| Question(e1, e2, e3) ->
      (match eval renv e1 with
         VNum(value1) ->
           let v2 = eval renv e2 in
           let v3 = eval renv e3 in
           (match value1 with
              0 ->
                (match v2 with
                   VTrue -> v3
                 | VFalse -> VFalse
                 | _ -> raise BugTypeInfer)
            | _ ->
                (match v2 with
                   VTrue -> VTrue
                 | VFalse -> v3
                 | _ -> raise BugTypeInfer))
       | _ -> raise BugTypeInfer)

  | Dollar(e1,e2) ->
    let v1 = eval renv e1 in
    let v2 =
      (match eval renv e2 with
         VNum(value) -> value
       | _ -> raise BugTypeInfer) in
    (match v1 with
       VTrue -> VNum(v2+1)
     | VFalse ->
         (match v2 with
            0 -> VNum(0)
          | _ -> VNum(v2-1))
     | _ -> raise BugTypeInfer) *)

(* auxiliar functions for prints *)
let rec ttos (t: tipo) : string =
  match t with
    TyInt         -> "int"
  | TyBool        -> "bool"
  | TyFn(t1,t2)   -> "("  ^ (ttos t1) ^ " --> " ^ (ttos t2) ^ ")"
  | TyPair(t1,t2) -> "("  ^ (ttos t1) ^ " * "   ^ (ttos t2) ^ ")"

let rec vtos (v: valor) : string =
  match v with
    VNum n -> string_of_int n
  | VBool b -> string_of_bool b
  | VPair(v1, v2) -> "(" ^ vtos v1 ^ "," ^ vtos v1 ^ ")"
  | VClos _ ->  "fn"
  | VRclos _ -> "fn"

(* main *)
let int_bse (e:expr) : unit =
  try
    let typ = typeinfer [] e in
    let value = eval [] e
    in  print_string ((vtos value) ^ " : " ^ (ttos typ))
  with
    TypeError msg -> print_string ("[erro de tipo] " ^ msg)

 (* as exceções abaixo nao podem ocorrer   *)
  | BugTypeInfer  ->  print_string "[bug - typeinfer]"
  | BugParser     ->  print_string "[bug - parser]"


(* +++++++++++++++++++++++++++++++++++++++*)
(*                TESTES                  *)
(*++++++++++++++++++++++++++++++++++++++++*)

(*
  let x:int = 2
    in let foo: int --> int = fn y:int => x + y
      in let x: int = 5
        in foo 10

  do tipo int, avalia para 12
*)

let e'' = Let("x", TyInt, Num 5, App(Var "foo", Num 10))

let e'  = Let("foo", TyFn(TyInt,TyInt), Fn("y", TyInt, Binop(Sum, Var "x", Var "y")), e'')

let tst = Let("x", TyInt, Num(2), e')

(*
  let x:int = 2
    in let foo: int --> int = fn y:int => x + y
      in let x: int = 5
        in foo

  do tipo int --> int, avalia para uma função
*)


let e2 = Let("x", TyInt, Num 5, Var "foo")

let e1  = Let("foo", TyFn(TyInt,TyInt), Fn("y", TyInt, Binop(Sum, Var "x", Var "y")), e2)

let tst2 = Let("x", TyInt, Num(2), e1)

(* let tst3 = Dollar(Num 5, Num 2) *)
