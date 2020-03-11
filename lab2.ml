(* lab 2 CSCI 2041, Isaiah Johnston joh14135 *)

let num = fst;; 
let den = snd;; 

let rec gcd i j = 
  if i <> 0 
  then if j > i 
       then gcd i (j - i) 
       else gcd (i - j) j 
  else j ;;

let rat n d = 
  let g = (gcd n d) in
  let n = (n/g) in
  let d = (d/g) in
  (n, d);;

let ratAdd a b = 
  rat (num(a)*den(b)+den(a)*num(b)) (den(a)*den(b));;

let ratMul a b = 
    rat (num(a)*num(b)) (den(a)*den(b));;

let ratDiv a b = 
  rat (num(a)*den(b)) (den(a)*num(b));;

let ratGt a b = 
  if (num(a)*den(b)) > (den(a)*num(b)) then
    true
  else 
    false;;

let euler epilson = 
    let c = rat 1 1 in
    let s = rat 0 1 in
    let t = rat 1 1 in
    let rec eulering c s t epilson =
      if (ratGt t epilson) = false then
         s
      else
      eulering (ratAdd c (1, 1)) (ratAdd s t) (ratDiv t c) epilson in

    eulering c s t epilson;;
