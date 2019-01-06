module Main

open FStar.All
open FStar.IO
open FStar.Order
open Mynat

let main = 30 |> mynat_of_nat |> print_mynat; print_newline ()
