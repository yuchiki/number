module MynatString

open FStar.All
open FStar.IO
open MynatDefinition

val string_of_mynat : mynat -> string
let rec string_of_mynat = function
        | Zero -> "Z"
        | Succ m -> "S" ^ string_of_mynat m

val print_mynat : mynat -> ML unit
let print_mynat n = n |> string_of_mynat |> print_string
