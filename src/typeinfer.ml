let rec lookup list key =
  match list with
    [] -> None
  | (k,v) :: tl -> if (k=key) then Some v else lookup tl key

let update list key value = (key,value) :: list

let rec memLength (mem: memory) : int =
  (match mem with
    (k, v)::tail -> 1 + memLength tail
    | [] -> 0)

let rec refreshMemAddr (mem: memory) (addr: address) (newValue: valor) : (valor * memory) = 
  (match mem with
      [] -> (VUnit (), [])
    | (key, value)::tail when key=addr -> (VUnit (), (addr, newValue) :: tail)
    | (key, value)::tail -> 
      (match (refreshMemAddr tail addr newValue) with
          (_, tailRefreshed) -> (VUnit (), (key, value) :: tailRefreshed)
        | _ -> raise BugParser))

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

    (* TSkip *)
  | Skip -> TyUnit

    (* TWhile *)
  | Whl (e1, e2) ->
    (match typeinfer tenv e1 with
        TyBool ->
          (match typeinfer tenv e2 with
              TyUnit -> TyUnit
            | _ -> raise (TypeError "e2 de Whl não é do tipo unit"))
      | _ -> raise (TypeError "condição de Whl não é do tipo bool"))

    (* TSeq *)
  | Seq (e1, e2) ->
    (match typeinfer tenv e1 with
        TyUnit -> typeinfer tenv e2
      | _ -> raise (TypeError "e1 de Seq não é do tipo bool"))

    (* TNew *)
  | New e -> TyRef(typeinfer tenv e)

    (* TDeref *)
  | Dref e ->
    (match typeinfer tenv e with
        TyRef(typ) -> typ
      | _ -> raise (TypeError "expressão para Dref não é do tipo ref"))

    (* TAtr *)
  | Asg (e1, e2) ->
    let type1 = typeinfer tenv e1 in
    (match type1 with
        TyRef(typ) ->
          let type2 = typeinfer tenv e2 in
          if typ = type2
            then TyUnit
            else raise (TypeError "tipos diferentes para Asg")
      | _ -> raise (TypeError "e1 para Asg não é do tipo ref"))
      