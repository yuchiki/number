module MynatAdd

open MynatDefinition
open Comparison

val mynatadd : mynat -> mynat -> mynat
let rec mynatadd m n = match m with
    | Zero -> n
    | Succ m1 -> mynatadd m1 (Succ n)

val mynatsuccplus : m:mynat -> n:mynat -> Lemma (mynatadd (Succ m) n = Succ (mynatadd m n))
let rec mynatsuccplus m n = match m with
    | Zero -> ()
    | Succ m1 -> mynatsuccplus m1 (Succ n)


val mynatidentr : m:mynat -> Lemma (mynatadd m Zero = m)
let rec mynatidentr = function
    | Zero -> ()
    | Succ m1 -> mynatsuccplus m1 Zero; mynatidentr m1


val mynatcommutativity : m:mynat -> n:mynat -> Lemma (mynatadd m n = mynatadd n m)
let rec mynatcommutativity m n = match m with
    | Zero -> mynatidentr n
    | Succ m1 -> mynatcommutativity m1 (Succ n)

val mynatassociativity : l:mynat -> m:mynat -> n:mynat -> Lemma (mynatadd (mynatadd l m) n = mynatadd l (mynatadd m n))
let rec mynatassociativity l m n = match l with
    | Zero -> ()
    | Succ l1 ->
        mynatsuccplus l1 m;
        mynatsuccplus (mynatadd l1 m) n;
        mynatsuccplus l1 (mynatadd m n);
        mynatassociativity l1 m n


val mynatsuccplusr : m:mynat -> n:mynat -> Lemma (mynatadd m (Succ n) = Succ (mynatadd m n))
let mynatsuccplusr m n =
    mynatcommutativity m (Succ n);
    mynatsuccplus n m;
    mynatcommutativity n m
