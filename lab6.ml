(*
  CSci 2041 Lab Assignment 6

    James Moen
    18 Oct 19

  It's worth 30 points.
*)

(* PROPOSITION. An expression in propositional logic using '¬', '∧', and '∨'.

   a, b, c ...  ↝  Var "a", Var "b", Var "c" ...
   α ∧ β        ↝  And (α, β)
   α ∨ β        ↝  Or (α, β)
   ¬ α          ↝  Not α

   The squiggly arrow '↝' means "represented as." *)

type proposition =
  Var of string |
  And of proposition * proposition |
  Or of proposition * proposition |
  Not of proposition ;;



(*

YOUR DEFINITION FOR UNORIFY GOES HERE!

*)

let rec unorify proposition =
  match proposition
  with 
  Var name -> Var name |
  Not(Var name) -> Not (Var name) |
  Not(Not right) -> unorify right |
  Or(left, right) ->  Not(And(unorify(Not left), unorify(Not right))) |
  Not(Or(left, right)) -> And(unorify(Not left), unorify (Not right)) |
  Not(And(left, right)) -> Not(And((unorify left), (unorify right))) |
  And(left, right) -> And((unorify left), (unorify right)) |
  Not right -> Not(unorify right);;



(* Unorify the proposition a. *)

unorify (Var "a");;

(* 2 points if you get: Var "a" *)



(* Unorify the proposition ¬ a. *)

unorify (Not(Var "a"));;

(* 3 points if you get: Not (Var "a") *)




(* Unorify the proposition ¬ ¬ a. *)

unorify (Not(Not(Var "a")));;

(* 5 points if you get: Var "a" *)




(* Unorify the proposition ¬ (a ∨ b). *)

unorify (Not(Or(Var "a", Var "b")));;

(* 5 points if you get: And (Not (Var "a"), Not (Var "b")) *)




(* Unorify the proposition ¬ (a ∧ b). *)

unorify (Not(And(Var "a", Var "b")));;

(* 5 points if you get: Not (And (Var "a", Var "b")) *)




(* Unorify the proposition ¬ a ∨ ¬ b. *)

unorify (Or(Not(Var "a"), Not(Var "b")));;

(* 5 points if you get: Not (And (Var "a", Var "b")) *)




(* Unorify the proposition ¬ ¬ a ∨ ¬ b. *)

unorify (Or((Not(Not(Var "a"))), (Not(Var "b"))));;

(* 5 points if you get: Not (And (Not (Var "a"), Var "b")) *)
