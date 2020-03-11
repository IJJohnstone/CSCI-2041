(*
  CSci 2041 Lab Assignment 9

    Isaiah Johnston
    03 Nov 19

  30 points.
*)

(* Define the function PRINTF, among other things. *)

open Printf ;;

(* THING. The type of a Pure Lisp object, from the lectures. *)

let rec printingThings things = 
  let rec printingThing thing =
    match thing 
    with Nil -> printf "nil" |
    Number n -> printf "%i" n |
    Symbol s -> printf "%s" s |
    Cons(first, rest) -> printf "("; printingThing first; printingThings rest; printf ")" in

    match things
    with Nil -> printf "" |
    Cons(first, rest) -> printf " "; printingThing first; printingThings rest;;

    
let rec printingThing thing =
    match thing 
    with Nil -> printf "nil" |
    Number n -> printf "%i" n |
    Symbol s -> printf "%s" s |
    Cons(first, rest) -> printf "("; printingThing first; printingThings rest; printf ")";;



let printThing thing =
  printingThing thing;
  printf "\n";;
