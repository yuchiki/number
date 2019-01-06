module Main

open FStar.All
open FStar.IO
open FStar.Order

(* definition of mynat *)
type mynat = | Zero | Succ : mynat -> mynat

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

(* comparation *)
val myleq : mynat -> mynat -> bool
let rec myleq m n = match (m, n) with
    | (Zero, _) -> true
    | (_, Zero) -> false
    | (Succ a, Succ b) -> myleq a b

val my_leq_or_geq: m:mynat -> n:mynat -> Lemma (myleq m n \/ myleq n m)
let rec my_leq_or_geq m n= match (m, n) with
    | (Zero, _) -> ()
    | (_, Zero) -> ()
    | (Succ a, Succ b) -> my_leq_or_geq a b


(* print mynat *)
val string_of_mynat : mynat -> string
let rec string_of_mynat = function
        | Zero -> "Z"
        | Succ m -> "S" ^ string_of_mynat m

val print_mynat : mynat -> ML unit
let print_mynat n = n |> string_of_mynat |> print_string

let main = 10 |> mynat_of_nat |> print_mynat; print_newline ()
