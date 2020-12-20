(* 7. Arbore echilibrat (AVL) Scrieți o funcție care determină dacă un arbore 
binar e echilibrat, în sensul că pentru fiecare nod, înălțimea celor doi 
subarbori diferă cu cel mult 1. *)

type 'a bintree = Nil | BT of 'a bintree * 'a * 'a bintree;; 

let b = BT(BT(BT(Nil,20,Nil), 30, BT(Nil,40,Nil)), 50, BT(BT(Nil,60,Nil),70,BT(Nil,80,BT(Nil,90,Nil))))

let dif_inaltime arbore1 arbore2 = 
  let rec aux arbore nr = match arbore with
    | Nil -> nr 
    | BT(s, r, d) -> max (aux s (nr + 1)) (aux d (nr + 1 ))
  in
  if (abs(aux arbore1 (-1) - aux arbore2 (-1)) > 1) then raise Exit else true
    

let rec echilibrat arbore = match arbore with
  | Nil -> true
  | BT(s, r, d) -> echilibrat s && echilibrat d && (try dif_inaltime s d with Exit -> false)
;;