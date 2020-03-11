
type proposition =
  False |
  True |
  Var of string |
  And of proposition * proposition |
  Or of proposition * proposition |
  Not of proposition |
  Imply of proposition * proposition |
  Equiv of proposition * proposition ;;

type conditional =
  IffyFalse |
  IffyTrue |
  IffyVar of string |
  If of conditional * conditional * conditional ;;


let rec ifify p =
  match p
  with 
  True -> IffyTrue |
  False -> IffyFalse |
  Var name -> IffyVar name |
  Not right -> If((ifify right), IffyFalse, IffyTrue) |
  And(left, right) -> If((ifify left), (ifify right), IffyFalse) |
  Or(left, right) -> If((ifify left), IffyTrue, (ifify right)) |
  Imply(left, right) -> If((ifify left), (ifify right), IffyTrue) |
  Equiv(left, right) -> If((ifify left), (ifify right), If((ifify right), IffyFalse, IffyTrue));;

let rec normalize c = 
  match c 
  with 
  IffyTrue -> IffyTrue |
  IffyFalse -> IffyFalse |
  IffyVar name -> IffyVar name |
  If(If(pi, alpha1, beta1), alpha2, beta2) -> 
          If((normalize pi), 
          If((normalize alpha1), (normalize alpha2), (normalize beta2)), 
          If((normalize beta1), (normalize alpha2), (normalize beta2))) |
  If(pi, alpha, beta) -> If((normalize pi), (normalize alpha), (normalize beta));;

let rec substitute c v b = 
    match c 
    with 
    IffyTrue -> IffyTrue |
    IffyFalse -> IffyFalse |
    IffyVar v -> b |
    If(pi, alpha, beta) -> If((substitute pi v b), (substitute alpha v b), (substitute beta v b));;
          

 let rec simplify c = 
  match c 
  with 
  IffyTrue -> IffyTrue |
  IffyFalse -> IffyFalse |
  IffyVar name -> IffyVar name |
  If(pi, alpha, beta) -> 
            let alpha' = (substitute alpha pi IffyTrue) in
            let beta' = (substitute beta pi IffyFalse) in
            if pi = IffyTrue then
                simplify alpha'
            else if pi = IffyFalse then
                simplify beta'
            else if alpha' = IffyTrue && beta' = IffyFalse then
                simplify pi
            else if alpha' = beta' then
                simplify alpha'
            else
              simplify ( If(simplify pi, simplify alpha', simplify beta'));;

let tautology p =
  let b = simplify(normalize(ifify p)) in
  if b = IffyTrue then
      true
  else 
      false;;
  
      



tautology ((Imply
    (Not
      (And
        (Var "p", Var "q")),
     Or
      (Not
        (Var "p"),
       Not
        (Var "q"))))) ;;   (* ¬ (p ∧ q) → (¬ p ∨ ¬ q)  this is a tautology so it should return true*)

tautology (Or(Not(Var "p"), Var "p"));; (* ¬ p ∨ p , this is a tautology so it should return true *)

tautology (Equiv(Not(And(Var "a", Var "b")), Or(Not(Var "a"), Not(Var "b"))));; (* this is De Morgans Law, should return true *)

tautology (Or(Var "p", Var "q"));; (* p ∨ q is not a tautology, so it should return false *)

tautology (And((Imply( Var "a", Var "b")), Var "c"));;  (* (a → b) ∧ c is not a tautology, so it should return false *)

tautology (And (Var "p", Var "q")) ;; (* p ∧ q is not a tautology, so the following expression returns false. *)

tautology (Equiv
            (Not
              (And(
                (Imply(Var "a", Var"b")), Var "c")), Or(Var "b", Var"c")));;        (* ¬((a → b) ∧ c) ↔ (b ∨ c) is not a tautology, returns false *)



