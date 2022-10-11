
type ident = string

type tipo =
    TyInt
  | TyBool
  | TyFn of tipo * tipo
  | TyPair of tipo * tipo

  (* extensões *)
  | TyRef of tipo
  | TyUnit

type tenv = (ident * tipo) list

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

  (* extensões *)
  | Asg of expr * expr
  | Dref of expr
  | New of expr
  | Seq of expr * expr
  | Whl of expr * expr
  | Skip

type address = int
type valor =
  | VNum of int
  | VBool of bool
  | VUnit of unit
  | VAddress of address
  | VPair of valor * valor
  | VClos  of ident * expr * renv
  | VRclos of ident * ident * expr * renv
and
  renv = (ident * valor) list

type memory = (address * valor) list

exception TypeError of string
exception RefError of string

(* bugs *)
exception NotImplemented of string
exception BugParser
exception BugTypeInfer of string
