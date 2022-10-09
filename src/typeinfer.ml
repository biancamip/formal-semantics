
let rec lookup a k =
  match a with
    [] -> None
  | (y,i) :: tl -> if (y=k) then Some i else lookup tl k

let update a k i = (k,i) :: a

let rec typeinfer (tenv:tenv) (e:expr) : tipo =
  match e with

    (* TInt  *)
  | Num _ -> TyInt

    (* TVar *)
  | Var x ->
      (match lookup tenv x with
        Some t -> t
       | None -> raise (TypeError ("variavel nao declarada: " ^ x)))

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

  | Skip -> TyUnit
  
  | Asg (_, _) -> raise (NotImplemented "Asg")
  | Dref _     -> raise (NotImplemented "Dref")
  | New _      -> raise (NotImplemented "New")
  | Seq (_, _) -> raise (NotImplemented "Seq")
  | Whl (_, _) -> raise (NotImplemented "Whl")
  