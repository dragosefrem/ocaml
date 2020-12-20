(* 9. Al n-lea nod - Scrieți o funcție care returnează al n-lea nod în inordine 
dintr-un arbore (numărând de la zero).  *)

type 'a tree = T of 'a * 'a tree list

let inorder_nth tree n =                       
  let rec inorder tree = match tree with
    | T(r, []) -> [r]
    | T(r, h :: t) -> List.append (List.append (inorder h) [r]) (List.fold_left (fun acc e -> inorder e) [] t)
  in
  List.nth (inorder tree) n;;