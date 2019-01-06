module MynatAdd

open MynatDefinition
open Comparison

val myadd : mynat -> mynat -> mynat
let rec myadd m n = match m with
    | Zero -> n
    | Succ m1 -> myadd m1 (Succ n)

val succplus : m:mynat -> n:mynat -> Lemma (myadd (Succ m) n = Succ (myadd m n))
let rec succplus m n = match m with
    | Zero -> ()
    | Succ m1 -> succplus m1 (Succ n)


val myidentr : m:mynat -> Lemma (myadd m Zero = m)
let rec myidentr = function
    | Zero -> ()
    | Succ m1 -> succplus m1 Zero; myidentr m1


val mycommutativity : m:mynat -> n:mynat -> Lemma (myadd m n = myadd n m)
let rec mycommutativity m n = match m with
    | Zero -> myidentr n
    | Succ m1 -> mycommutativity m1 (Succ n)

val myassociativity : l:mynat -> m:mynat -> n:mynat -> Lemma (myadd (myadd l m) n = myadd l (myadd m n))
let rec myassociativity l m n = match l with
    | Zero -> ()
    | Succ l1 ->
        succplus l1 m;
        succplus (myadd l1 m) n;
        succplus l1 (myadd m n);
        myassociativity l1 m n
