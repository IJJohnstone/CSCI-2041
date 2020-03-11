(* lab 4, Isaiah Johnston, joh14135 *)

type 'base mutyQueue = 
  MutyQueueNode 
  of 'base ∗ 
     'base mutyQueue ref ∗ 
     'base mutyQueue ref ;;

let rec mutyQueueMake s = 
  MutyQueueNode(s, mutyQueueMake s, mutyQueueMake s);;
  
let mutyQueueEmpty q =

let mutyQueueEnqueue q e =

let mutyQueueDequeue q =