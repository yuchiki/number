module MyintDefinition

open FStar.All

open MynatDefinition
open MynatAdd
open MynatNat

type myint = mynat * mynat

val normalize : n:myint -> Tot myint (decreases %[nat_of_mynat (fst n)])
let rec normalize = function
    | (Zero, r) -> (Zero, r)
    | (l, Zero) -> (l, Zero)
    | (Succ l,  Succ r) -> normalize (l, r)

val myintsamediffsamenorm : m:mynat -> n:mynat -> d:mynat -> Lemma (normalize (m, n) = normalize (mynatadd m d, mynatadd n d))
let rec myintsamediffsamenorm l r d = match d with
    | Zero -> mynatidentr l; mynatidentr r
    | Succ d1 ->
        mynatsuccplusr l d1;
        mynatsuccplusr r d1;
        myintsamediffsamenorm l r d1

val myinteq : myint -> myint -> bool
let myinteq m n = normalize m = normalize n

val myintzero : myint
let myintzero = (Zero, Zero)

val myintinzero : myint -> bool
let myintiszero n = myinteq n myintzero
