
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


let rec eval (renv: renv) (e: expr) (mem: memory) : (valor * memory) =
  match e with

    Num n -> (VNum n, mem)
  | Bcte b -> (VBool b, mem)

  | Var x ->
      (match lookup renv x with
        Some v -> (v, mem)
       | None -> raise BugTypeInfer)

  | Binop(operand,e1,e2) ->
      let (value1, mem1) = eval renv e1 mem in
      let (value2, mem2) = eval renv e2 mem1 in
        ((computeBinOp operand value1 value2), mem2)

  | Pair(e1, e2) ->
      let (value1, mem1) = eval renv e1 mem in
      let (value2, mem2) = eval renv e2 mem1 in
        (VPair(value1, value2), mem2)

  | Fst e ->
      let (value, memRes) = eval renv e mem in
      (match value with
          VPair(v1, _) -> (v1, memRes)
        | _ -> raise BugTypeInfer)

  | Snd e ->
      let (value, memRes) = eval renv e mem in
      (match value with
          VPair(_, v2) -> (v2, memRes)
        | _ -> raise BugTypeInfer)

  | If(e1, e2, e3) ->
      let (value1, mem1) = eval renv e1 mem in
      (match value1 with
         VBool true  -> eval renv e2 mem1
       | VBool false -> eval renv e3 mem1
       | _ -> raise BugTypeInfer)

  | Fn(x,_,e1) -> (VClos(x,e1,renv), mem)

  | App(e1, e2) ->
      let (value1, mem1) = eval renv e1 mem in
      let (value2, mem2) = eval renv e2 mem1 in
      (match value1 with
         VClos(x,ebdy,renv') ->
          let renv'' = update renv' x value2
          in eval renv'' ebdy mem2

       | VRclos(f,x,ebdy,renv') ->
          let renv''  = update renv' x value2 in
          let renv''' = update renv'' f value1
          in eval renv''' ebdy mem2

       | _ -> raise BugTypeInfer)

  | Let(x, _, e1, e2) ->
      let (value1, mem1) = eval renv e1 mem
      in eval (update renv x value1) e2 mem1

  | LetRec(f, TyFn(t1,t2), Fn(x,tx,e1), e2) when t1 = tx ->
      let renv'= update renv f (VRclos(f,x,e1,renv))
      in eval renv' e2 mem
  | LetRec _ -> raise BugParser

  (* skip;e2, σ −→ e2, σ *)
  | Skip -> (VUnit (), mem)

  (* while e1 do e2, σ −→ if e1 then (e2;while e1 do e2) else skip, σ *)
  | Whl (e1, e2) -> 
    let (value1, mem1) = eval renv e1 mem in
    (match value1 with
        VBool true -> 
          let exp = Seq(e2, Whl(e1,e2))
          in eval renv exp mem1
      | VBool false -> eval renv Skip mem1
      | _ -> raise BugTypeInfer)
  
  | Seq (e1, e2) -> 
    let (value1, mem1) = eval renv e1 mem in
    (match value1 with
        VUnit _ -> eval renv e2 mem1
      | _ -> raise BugTypeInfer)

  | Asg (_, _) -> raise (NotImplemented "Asg")
  | Dref _     -> raise (NotImplemented "Dref")
  | New _      -> raise (NotImplemented "New")