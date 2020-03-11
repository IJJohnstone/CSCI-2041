let lisp path = 
  let nextThing = makeParser path in
  let rec lisping thing =
    match thing
    with Symbol "End" -> () |
    _ -> printThing(eval(nextThing()));
         lisping(nextThing())
    in lisping(nextThing());;

lisp "tests123.txt";;