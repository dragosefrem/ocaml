(* Exercițiul 6: Operații cu funcții 
În matematică, am extins uneori operatorul + de la numere la funcții, definind funcția f + g 
prin relația (f + g)(x) = f(x) + g(x)  *)


(* a) Definiți în ML o funcție care ia ca parametru două funcții f și g și returnează funcția definită 
ca suma lor prin relația de mai sus. *)
    
let sum_functions f g x = f x + g x 
  
(* b) Scrieți o funcție mai generală, care primește ca parametru și 
operatorul binar (o funcție de două argumente) care e aplicată (valorilor) 
celor două funcții. Verificați că o puteți folosi cu operatorul (+) și ( pentru 
a calcula suma si produsul *)

let operate_functions op f g x = op (f x) (g x)

(* Test zone *)

let f x = x + 1
let g x = x - 1
let t0 = sum_functions f g 9 (* Rezultat asteptat 18 *)
let t1 = sum_functions f g 0 (* Rezultat asteptat 0 *)
let t2 = operate_functions (+) f g 9 (* Rezultat asteptat 18 *)
let t3 = operate_functions (+) f g 3 (* Rezultat asteptat 6 *)
let t4 = operate_functions ( * ) f g 9 (* Rezultat asteptat 80 *)
let t5 = operate_functions ( * ) f g 0 (* Rezultat asteptat -1 *)


;;