(* 10. Un calculator memorează numere în doi regiștri a și b și poate executa 
la fiecare pas una din două instrucțiuni: a += b care adună pe b la a 
(modificând pe a) și b += a (analog). Scrieți o funcție care ia ca parametri 
două numere naturale a și b și o valoare limită lim și calculează mulțimea 
tuturor numerelor care se pot obține cu aceste instrucțiuni pornind de la a 
și b, nedepășind lim . *) 

module Int = struct
  type t = int
  let compare = compare
end 

module IntSet = Set.Make(Int)

let mul_instruct a b lim = 
  let rec sub a b lim result = 
    if a <= lim && b <= lim then IntSet.union (sub (a+b) b lim (IntSet.add a (IntSet.add b IntSet.empty))) (sub a (b + a) lim (IntSet.add a (IntSet.add b IntSet.empty)))
    else result
  in
  sub a b lim IntSet.empty
;;