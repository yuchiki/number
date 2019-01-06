module MynatNat

open MynatDefinition

(* mynat and nat are isomorphism *)
val mynat_of_nat : nat -> mynat
let rec mynat_of_nat n = if n = 0 then Zero else Succ (mynat_of_nat (n-1))

val nat_of_mynat : mynat -> nat
let rec nat_of_mynat = function
    | Zero -> 0
    | Succ n -> 1 + nat_of_mynat n

val nat_of_mynat_of_nat_is_id : n:nat -> Lemma (nat_of_mynat (mynat_of_nat n) = n)
let rec nat_of_mynat_of_nat_is_id = function
    | 0 -> ()
    | n -> nat_of_mynat_of_nat_is_id (n - 1)

val mynat_of_nat_of_mynat_is_id : n:mynat -> Lemma (mynat_of_nat (nat_of_mynat n) = n)
let rec mynat_of_nat_of_mynat_is_id = function
    | Zero -> ()
    | Succ n -> mynat_of_nat_of_mynat_is_id n
