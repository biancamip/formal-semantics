type ident = string
type tipo =
  | TyInt
  | TyBool
  | TyFn of tipo * tipo
  | TyPair of tipo * tipo

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

type tenv = (ident * tipo) list

type valor =
  | VNum of int
  | VBool of bool
  | VPair of valor * valor
  | VClos  of ident * expr * renv
  | VRclos of ident * ident * expr * renv
and
  renv = (ident * valor) list
  
(* warn user *)
exception TypeError of string

(* these should never be raised  *)
exception BugParser
exception BugTypeInfer
