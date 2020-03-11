(*
   PARSE. An OCaml function that acts as a parser for Pure Lisp.

   James Moen
   01 Dec 19

   This needs MAKE SCANNER.
*)



(* CAN'T PARSE. Raised if the parser fails. *)

exception Can'tParse of string ;;

(* MAKE PARSER. Return a parser that reads THINGs from the file whose pathname
   is PATH. OCaml raises an exception if there's no such file. *)

   let makeParser path =
    let nextToken = makeScanner path
  
  (* NEXT THING. Read the THING whose first token is TOKEN, and return it. *)
  
    in let rec nextThing token =
  
  (* NEXT THINGS. Read a series of THINGs as a Pure Lisp list, and return that
     list. *)
  
         let rec nextThings token =
           match token
           with CloseParenToken ->
                  Nil |
  
                EndToken ->
                  raise (Can'tParse "Unexpected end of file.") |
  
                _ ->
                  let first = nextThing token
                  in let rest = nextThings (nextToken ())
                     in Cons (first, rest)
  
  (* This is NEXT THINGS's body. *)
  
         in match token
            with CloseParenToken ->
                   raise (Can'tParse "Unexpected close parenthesis.") |
  
                 EndToken ->
                   raise (Can'tParse "Unexpected end of file.") |
  
                 NumberToken integer ->
                   Number integer |
  
                 OpenParenToken ->
                   nextThings (nextToken ()) |
  
                 SymbolToken string ->
                   Symbol string
  
  (* This is NEXT THING's body. *)
  
       in (fun () -> nextThing (nextToken ())) ;;
