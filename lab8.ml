let makeStream this state next = 
  ((this, state), next) ;; 
 
let first ((this, state), next) = 
  this ;; 
 
let rest ((this, state), next) = 
  (next this state, next) ;;

let advance predicate stream = 
    