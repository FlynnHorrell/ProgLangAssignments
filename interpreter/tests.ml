open Types

(* You can test expressions of type resultS or resultC and how they are evaluated *)
(* These will only work once you have compiled types.ml *)

(* This is the one kind of test you can write. *)
let t0a = evaluate (NumC 2.3) = Num 2.3

(* You can also use interp directly to specify a custom environment. *)
let t0b = let env1 = bind "x" (Num 3.1) empty
          in interp env1 (NumC 2.3) = Num 2.3

(* You can also test desugar to make sure it behaves properly. *)
let t0c = desugar (NumS 2.3) = NumC 2.3

(* Or you can combine with evaluate to get to the final value. *)
let t0d = evaluate (desugar (NumS 2.3)) = Num 2.3


let t1a = evaluate (BoolC true) = Bool true
let t1b = evaluate (BoolC false) = Bool false
let t1c = desugar (BoolS true) = BoolC true
let t1d = desugar (BoolS false) = BoolC false

let t2a = evaluate (IfC (BoolC true, NumC 2.3, NumC 4.3)) = Num 2.3
let t2b = evaluate (IfC (IfC (BoolC false, BoolC false, BoolC true), NumC 2.7, NumC 4.3)) = Num 2.7
let t2c = desugar (IfS (BoolS true, NumS 2.3, NumS 4.3)) = IfC(BoolC true, NumC 2.3, NumC 4.3)
let t2d = evaluate (desugar (NotS (BoolS true))) = Bool false
let t2e = evaluate (desugar (NotS (BoolS false))) = Bool true
let t2f = evaluate (desugar (OrS (BoolS true, BoolS true))) = Bool true
let t2g = evaluate (desugar (OrS (BoolS false, BoolS true))) = Bool true
let t2h = evaluate (desugar (OrS (BoolS false, BoolS false))) = Bool false
let t2i = evaluate (desugar (AndS (BoolS false, BoolS true))) = Bool false
let t2j = evaluate (desugar (AndS (BoolS true, BoolS false))) = Bool false
let t2k = evaluate (desugar (AndS (BoolS true, BoolS true))) = Bool true

let t3a = evaluate (ArithC ("+", NumC 1., NumC 1.)) = Num 2.
let t3b = evaluate (ArithC ("-" , NumC 2. , NumC 1.)) = Num 1.
let t3c = evaluate (ArithC ("*" , NumC 5. , NumC 5.)) = Num 25.
let t3d = evaluate (ArithC ("/" , NumC 10. , NumC 1.)) = Num 10.
let t3e = desugar (ArithS ("+", NumS 1., NumS 1.)) = ArithC("+", NumC 1., NumC 1.)

let t4a = evaluate (CompC (">", NumC 20., NumC 1.)) = Bool true 
let t4b = evaluate (CompC (">", NumC 1., NumC 20.)) = Bool false
let t4c = evaluate (CompC (">=", NumC 20., NumC 20.)) = Bool true 
let t4d = evaluate (CompC (">=", NumC 0., NumC 1.)) = Bool false 
let t4e = evaluate (CompC ("<", NumC 1., NumC 1.)) = Bool false 
let t4f = evaluate (CompC ("<", NumC 0., NumC 1.)) = Bool true 
let t4g = evaluate (CompC ("<=", NumC 20., NumC 100.)) = Bool true 
let t4h = evaluate (CompC ("<=", NumC 20., NumC 1.)) = Bool false