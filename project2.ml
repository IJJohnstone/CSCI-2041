(*

  Project 2, Scanner for Pure Lisp

    Isaiah Johnston
    17 Nov 19

*)


type token =
  CloseParenToken |
  EndToken |
  NumberToken of int |
  OpenParenToken |
  SymbolToken of string ;;

(* MAKE SCANNER. Return a function NEXT TOKEN that reads TOKENs from a file
   with pathname PATH. OCaml RAISEs an exception if there's no such file. *)

let makeScanner path =

(* INPUT. Read chars from this channel. *)

  let input = (open_in path)
  in

(* CH. The char most recently read from INPUT. *)

  let ch = ref ' ' in

(* NEXT CHAR. Advance CH to the next char from INPUT, or to '\000' if we're at
   the end of INPUT. *)

  let nextChar () =
    try ch := input_char input
    with End_of_file -> ch := '\000' in

  let nextEndToken () =
      EndToken in

  let nextOpenParenToken () = 
    nextChar();
    OpenParenToken in

  let nextCloseParenToken () =
    nextChar();
    CloseParenToken in

  let nextNumberToken prefix =
    let s = prefix in
    let rec nextNumberTokening s =
      match !ch
      with '\000' -> NumberToken (int_of_string s) |
      ' ' -> NumberToken (int_of_string s) |
      '\n' -> NumberToken (int_of_string s) |
      '(' -> NumberToken (int_of_string s) |
      ')' -> NumberToken (int_of_string s) |
      ',' -> NumberToken (int_of_string s) |
      _ -> let ch' = Char.escaped !ch in
           nextChar(); 
           nextNumberTokening (s^ch') in 
      nextNumberTokening s in

  let nextSymbolToken prefix =
    let s = prefix in
    let rec nextSymbolTokening s =
      match !ch
      with '\000' -> SymbolToken s |
      ' ' -> SymbolToken s |
      '\n' -> SymbolToken s |
      '(' -> SymbolToken s |
      ')' -> SymbolToken s |
      ',' -> SymbolToken s |
      _ -> let ch' = Char.escaped !ch in
           nextChar (); 
           nextSymbolTokening (s^ch') in
      nextSymbolTokening s in
      
  let nextNumberOrSymbolToken () = 
    nextChar ();
    match !ch
    with '1' -> nextNumberToken "-" |
    '2' -> nextNumberToken "-" |
    '3' -> nextNumberToken "-" |
    '4' -> nextNumberToken "-" |
    '5' -> nextNumberToken "-" |
    '6' -> nextNumberToken "-" |
    '7' -> nextNumberToken "-" |
    '8' -> nextNumberToken "-" |
    '9' -> nextNumberToken "-" |
    '0' -> nextNumberToken "-" |
    _ -> nextSymbolToken "-" in

  let rec nextToken () =
    match !ch
    with '\000' -> nextEndToken () |
    '\n' -> nextChar (); nextToken () |
    ' ' -> nextChar (); nextToken () |
    '(' -> nextOpenParenToken () |
    ')' -> nextCloseParenToken () |
    '-' -> nextNumberOrSymbolToken () |
    '1' -> nextNumberToken "" |
    '2' -> nextNumberToken "" |
    '3' -> nextNumberToken "" |
    '4' -> nextNumberToken "" |
    '5' -> nextNumberToken "" |
    '6' -> nextNumberToken "" |
    '7' -> nextNumberToken "" |
    '8' -> nextNumberToken "" |
    '9' -> nextNumberToken "" |
    '0' -> nextNumberToken "" |
    _ -> nextSymbolToken "" in


(* Finally initialize CH, and return NEXT TOKEN as promised. *)

    nextChar () ;
     nextToken ;;

(* NEXT TOKENS. Test the token scanner by reading tokens from the file whose
   pathname is PATH, and writing one-line descriptions of each token. *)

let nextTokens path =
  let nextToken = makeScanner path
  in let rec nextTokensing token =
       match token
       with CloseParenToken ->
              Printf.printf "CloseParenToken\n" ;
              nextTokensing (nextToken ()) |

            EndToken ->
              Printf.printf "EndToken\n" |

            NumberToken number ->
              Printf.printf "NumberToken %i\n" number ;
              nextTokensing (nextToken ()) |
 
            OpenParenToken ->
              Printf.printf "OpenParenToken\n" ;
              nextTokensing (nextToken ()) |

            SymbolToken string ->
              Printf.printf "SymbolToken \"%s\"\n" string ;
              nextTokensing (nextToken ())

     in nextTokensing (nextToken ()) ;;

