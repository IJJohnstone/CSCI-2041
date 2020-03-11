open List;;

let rec member e l =
     if l = [] then
        false
     else if e = hd(l) then
         true
     else
         member e (tl(l));;

let rec delete e l =
  if l = [] then
    l
  if e = hd(l) then
    delete e (tl(l))
  else
    delete e (hd(l))::(delete tl(l));;


  
  

