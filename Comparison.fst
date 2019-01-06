module Comparison

open MynatDefinition

val myleq : mynat -> mynat -> bool
let rec myleq m n = match (m, n) with
    | (Zero, _) -> true
    | (_, Zero) -> false
    | (Succ a, Succ b) -> myleq a b

val myconnex: m:mynat -> n:mynat -> Lemma (myleq m n \/ myleq n m)
let rec myconnex m n= match (m, n) with
    | (Zero, _) -> ()
    | (_, Zero) -> ()
    | (Succ a, Succ b) -> myconnex a b
