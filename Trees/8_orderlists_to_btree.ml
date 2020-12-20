(* Arbore din liste - Un arbore binar cu valori diferite în fiecare nod poate 
fi reconstruit în mod unic cunoscând lista nodurilor sale în inordine și 
preordine. Scrieți o funcție care construiește arborele, fiind date cele două liste.*)

type 'a bintree = Nil | BT of 'a bintree * 'a * 'a bintree

let rec listsToBT listInorder listPreorder = 
  let rec getIndex list el nr = match list with
    | [] -> (-1)
    | h :: t -> if h = el then nr else getIndex t el (nr + 1)
  in
  let currentRoot = try List.hd listPreorder with _ -> min_int in 
  let pozRoot = getIndex listInorder currentRoot 1 in 
  let t1 = try (List.filter (fun e -> getIndex listInorder e 1 < pozRoot) listInorder, List.filter (fun e -> getIndex listInorder e 1 > (if pozRoot = -1 then max_int else pozRoot)) listInorder) with _ -> ([],[]) in
  let t2 = try (List.filter (fun e -> getIndex (List.tl listPreorder) e 1 < pozRoot) (List.tl listPreorder), List.filter (fun e -> getIndex (List.tl listPreorder) e 1 >= (if pozRoot = -1 then max_int else pozRoot)) (List.tl listPreorder)) with _ -> ([],[]) in 
  
  if currentRoot = min_int || not (List.mem currentRoot listInorder) then Nil
  else match t1,t2 with (a,b),(c,d) -> BT(listsToBT a c,currentRoot,listsToBT b d)