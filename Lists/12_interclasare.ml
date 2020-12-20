(* 12. Scrieți o funcție care interclasează două liste, 
fiecare ordonată crescător, adică returnează lista cu elementele 
din ambele liste, ordonate. *)

let  interclaseaza l1 l2 = 
  let rec sub1 l1 l2 result = match l1, l2 with 
    | [], [] -> result
    | h1 :: t1, h2 :: t2 -> if h1 < h2 then sub1 t1 l2 (h1 :: result) else sub1 l1 t2 (h2 :: result)
    | [], h2 :: t2 ->  sub1 l1 t2 (h2 :: result)
    | h1 :: t1, [] -> sub1 t1 l2 (h1 :: result)
  in 
  List.rev (sub1 l1 l2 []);;