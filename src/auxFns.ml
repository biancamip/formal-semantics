
(* type to string *)
let rec ttos (t: tipo) : string =
  match t with
    TyInt         -> "int"
  | TyBool        -> "bool"
  | TyUnit        -> "unit"
  | TyFn(t1,t2)   -> "("  ^ (ttos t1) ^ " --> " ^ (ttos t2) ^ ")"
  | TyPair(t1,t2) -> "("  ^ (ttos t1) ^ " * "   ^ (ttos t2) ^ ")"

(* valor to string *)
let rec vtos (v: valor) : string =
  match v with
    VNum n -> string_of_int n
  | VBool b -> string_of_bool b
  | VUnit _ -> "()"
  | VPair(v1, v2) -> "(" ^ vtos v1 ^ "," ^ vtos v1 ^ ")"
  | VClos _ ->  "fn"
  | VRclos _ -> "fn"

(* mem to string *)
let rec mtos (mem: memory) : string =
  match mem with
    (k, v)::tail -> "(" ^ (string_of_int k) ^ ", " ^ (vtos v) ^ ") - " ^ (mtos tail)
    | [] -> "EOM"
