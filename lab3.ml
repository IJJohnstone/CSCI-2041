(* lab 3, Isaiah Johnston, joh14135 *)


let rec bstDelete tree key = 
    match tree
    with BstEmpty -> BstEmpty 
    | BstNode(otherkey, BstEmpty, BstEmpty) -> BstEmpty
    | BstNode(otherkey, BstEmpty, rightSubtree) -> rightSubtree
    | BstNode(otherkey, leftSubtree, BstEmpty) -> leftSubtree
    | BstNode(otherkey, leftSubtree, rightSubtree) -> 
          if key < otherkey then 
            BstNode(otherkey, (bstDelete leftSubtree key), rightSubtree)
          else if key > otherkey then
            BstNode(otherkey, leftSubtree, (bstDelete rightSubtree key))
          else 
            let newVal = bstMaxKey leftSubtree in
            BstNode(newVal, (bstDelete leftSubtree newVal), rightSubtree);;
