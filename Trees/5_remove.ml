(* 5. Eliminarea unui nod - Scrieți o funcție care ia ca parametru o valoare și 
   un arbore binar de căutare și returnează arborele din care valoarea a fost 
   eliminată (dacă exista).*)

type 'a bintree = Nil | BT of 'a bintree * 'a * 'a bintree;; 

let b = BT(BT(BT(Nil,20,Nil), 30, BT(Nil,40,Nil)), 50, BT(BT(Nil,60,Nil),70,BT(Nil,80,Nil)))

let elim_nod x b1 = 
  let rec min_arbore_drept b1 x = match b1 with
    | Nil -> x
    | BT(s, r, d) -> min_arbore_drept s r
  in
  let rec max_arbore_stang b1 x = match b1 with
    | Nil -> x
    | BT(s, r, d) -> max_arbore_stang d r
  in
  let rec cross x b1 ignore = match b1 with
    | Nil -> Nil
    | BT(s, r, d) -> 
        if r = ignore then Nil 
        else if r != x then BT(cross x s ignore, r , cross x d ignore)
        else (match (min_arbore_drept d max_int), (max_arbore_stang s min_int) with
            | (a, b) when a = max_int && b = min_int-> Nil
            | (a, b) when a = max_int -> BT(cross x s b , b, cross x d ignore)
            | (a, b) -> BT(cross x s ignore, a, cross x d a)
          )   
          
  in
  cross x b1 min_int;;