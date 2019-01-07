module MyintAdd

open MyintDefinition
open MynatAdd

val myintadd : myint -> myint -> myint
let myintadd (l1, r1) (l2, r2) = (mynatadd l1 l2, mynatadd r1 r2)

val myintidentl : n:myint -> z:myint{myintiszero z} -> Lemma (myinteq (myintadd z n) n)
let myintidentl (ln, rn) (lz, rz) = admit ()

val myintidentr : n:myint -> z:myint{myintiszero z} -> Lemma (myinteq (myintadd n z) n)
let myintidentr n z = admit ()
