exception Desugar of string      (* Use for desugarer errors *)
exception Interp of string       (* Use for interpreter errors *)

(* You will need to add more cases here. *)
type exprS = NumS of float
            |BoolS of bool
            |IfS of exprS * exprS * exprS
            |OrS of exprS * exprS
            |AndS of exprS * exprS
            |NotS of exprS 
            |ArithS of string * exprS * exprS
            |CompS of string * exprS * exprS

(* You will need to add more cases here. *)
type exprC = NumC of float
			|BoolC of bool 
      |IfC of exprC * exprC * exprC
      |ArithC of string * exprC * exprC
      |CompC of string * exprC * exprC
      |EqC of exprC * exprC


(* You will need to add more cases here. *)
type value = Num of float
			|Bool of bool 

type 'a env = (string * 'a) list
let empty = []

(* lookup : string -> 'a env -> 'a option *)
let rec lookup str env = match env with
  | []          -> None
  | (s,v) :: tl -> if s = str then Some v else lookup str tl
(* val bind :  string -> 'a -> 'a env -> 'a env *)
let bind str v env = (str, v) :: env


(*
   HELPER METHODS
   You may be asked to add methods here. You may also choose to add your own
   helper methods here.
*)

let arithEval op v1 v2 = 
  match (v1, v2) with
  | (Num x, Num y) -> (match op with 
                        | "+" -> Num (x +. y)
                        | "-" -> Num (x -. y)
                        | "*" -> Num (x *. y)
                        | "/" -> if y = 0.
                                 then raise (Interp "Division by Zero")
                                 else Num (x /. y)
                        | _ -> raise (Interp "Not an operator"))
  | _ -> raise (Interp "Not a Num")

let compEval op v1 v2 = 
  match (v1, v2) with
  | (Num x, Num y) -> (match op with
                       | ">" -> Bool (x > y)
                       | ">=" -> Bool (x >= y)
                       | "<" -> Bool (x < y)
                       | "<=" -> Bool (x <= y)
                       | _ -> raise (Interp "Not an operator"))
  | _ -> raise (Interp "Not a Num")

let eqVal v1 v2 = 
  match (v1,v2) with
  | (Num x, Num y) -> Bool (x = y)
  | (Bool b1, Bool b2) -> Bool (b1 = b2)
  | _ -> Bool false



(* INTERPRETER *)

(* You will need to add cases here. *)
(* desugar : exprS -> exprC *)
let rec desugar exprS = match exprS with
  | NumS i        -> NumC i
  | BoolS b       -> BoolC b
  | IfS (a, b, c) -> IfC (desugar a, desugar b, desugar c)
  | NotS e        -> IfC (desugar e, BoolC false,BoolC true)
  | OrS (e1, e2)  -> IfC (desugar e1, BoolC true, IfC (desugar e2,BoolC true,BoolC false))
  | AndS (e1,e2)  -> IfC (desugar e1, IfC(desugar e2,BoolC true,BoolC false),BoolC false)
  | ArithS (s,e1,e2) -> ArithC (s,desugar e1, desugar e2)
  | CompS (s,e1,e2) -> CompC (s,desugar e1, desugar e2)


(* You will need to add cases here. *)
(* interp : Value env -> exprC -> value *)
let rec interp env r = match r with
  | NumC i        -> Num i
  | BoolC b 	  -> Bool b
  | IfC (test, op1, op2 ) -> 
     (match (interp env test) with 
     | Bool true -> interp env op1
     | Bool false -> interp env op2
     | _ -> raise (Interp "Not a Bool")) 
  | ArithC (op, v1, v2) -> arithEval op (interp env v1) (interp env v2)
  | CompC (op, v1, v2) -> compEval op (interp env v1) (interp env v2)
  | EqC (v1, v2) -> eqVal (interp env v1) (interp env v2)

(* evaluate : exprC -> val *)
let evaluate exprC = exprC |> interp []




(* You will need to add cases to this function as you add new value types. *)
let rec valToString r = match r with
  | Num i           -> string_of_float i
  | Bool b 			    -> string_of_bool b
