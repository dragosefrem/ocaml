(* 1. Lista nodurilor 
   
     a) Scrieți o funcție care construiește lista nodurilor unui arbore binar, enumerate în i) preordine; ii) inordine; iii) postordine. *)

type 'a bintree = Nil | BT of 'a bintree * 'a * 'a bintree
                                
let bt = BT (BT (BT (Nil, "D", Nil), "B", Nil), "A", BT (BT (Nil, "E", Nil), "C", BT (Nil, "F", Nil)))
  
(*

preorder: A, B, D, C, E, F
inorder: D, B, A, E, C, F
postorder: D, B, E, F, C, A

*)
  
let rec preorder btree = match btree with
  | Nil -> []
  | BT(s,r,d) -> r :: List.append (preorder s) (preorder d) 
      
let rec inorder btree = match btree with 
  | Nil -> []
  | BT(s,r,d) -> List.append(List.append (inorder s) [r]) (inorder d)
        
let rec postorder btree = match btree with
  | Nil -> []
  | BT(s,r,d) -> List.append (List.append (postorder s) (postorder d)) [r]         
  
(* b) Scrieți o funcție care ia un arbore binar și returnează lista 
nodurilor care au un singur fiu. Ordinea nodurilor în listă va fi cea din 
traversarea în inordine. *)

let rec onlyChild btree = match btree with
  | Nil -> []
  | BT(s,r,d) -> List.append (List.append (onlyChild s) (if (s = Nil && d <> Nil) || (s <> Nil && d = Nil) then [r] else [])) (onlyChild d)

;;