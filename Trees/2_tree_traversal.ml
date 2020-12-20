(* 2. Parcurgerea arborilor oarecare Modificați funcțiile de parcurgere așa 
încât să funcționeze pe arbori oarecare (fiecare nod are o listă de fii). *)


type 'a tree = T of 'a * 'a tree list
                      
let t = T (7, [T (2, []); T (10, []); T (6, [T (5, []); T (11, [])])])
                      
(* preorder: 7, 2, 10, 6, 5, 11
inorder: 2, 7, 10, 5, 6, 11
postorder: 2, 10, 5, 11, 6, 7 *)

                      
let rec preorder1 tree = match tree with
  | T(r, []) -> [r]
  | T(r, l) -> r :: (List.fold_left (fun acc e -> List.append acc (preorder1 e)) [] l)
                    
let rec inorder1 tree = match tree with
  | T(r, []) -> [r]
  | T(r, h :: t) -> List.append (List.append (inorder1 h) [r]) (List.fold_left (fun acc e -> List.append acc (inorder1 e)) [] t)
let rec postorder1 tree = match tree with
  | T(r, []) -> [r]
  | T(r, l) -> List.append(List.fold_left (fun acc e -> List.append acc (postorder1 e)) [] l) [r]
               
;;