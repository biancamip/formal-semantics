
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

