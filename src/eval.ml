
let rec computeBinOp (operation: op) (v1: valor) (v2: valor) : valor =
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
      computeBinOp operand v1 v2


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

  | Skip -> VUnit ()
  
  | Asg (_, _) -> raise (NotImplemented "Asg")
  | Dref _     -> raise (NotImplemented "Dref")
  | New _      -> raise (NotImplemented "New")
  | Seq (_, _) -> raise (NotImplemented "Seq")
  | Whl (_, _) -> raise (NotImplemented "Whl")