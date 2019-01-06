module Comparison

open MynatDefinition

val myleq : mynat -> mynat -> bool
let rec myleq m n = match (m, n) with
    | (Zero, _) -> true
    | (_, Zero) -> false
    | (Succ a, Succ b) -> myleq a b

val myantisymmetry: m:mynat -> n:mynat{myleq m n && myleq n m} -> Lemma ( m = n)
let rec myantisymmetry m n = match (m, n) with
    | (Zero, Zero) -> ()
    | (Succ m1, Succ n1) -> myantisymmetry m1 n1

val mytransitivity : l:mynat -> m:mynat{myleq l m} -> n:mynat{myleq m n} -> Lemma (myleq l n)
let rec mytransitivity l m n = match (l, m, n) with
    | (Zero, _, _) -> ()
    | (Succ l1, Succ m1, Succ n1) -> mytransitivity l1 m1 n1

val myconnex: m:mynat -> n:mynat -> Lemma (myleq m n \/ myleq n m)
let rec myconnex m n= match (m, n) with
    | (Zero, _) -> ()
    | (_, Zero) -> ()
    | (Succ a, Succ b) -> myconnex a b
