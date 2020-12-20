(* 14. Sortați o listă prin interclasare (mergesort), despărțind lista în 
două jumătăți, sortând recursiv fiecare din ele, și apoi interclasând cele 
două liste sortate. Folosiți funcțiile din cele două probleme anterioare. *)


let  interclaseaza l1 l2 = 
  let rec sub1 l1 l2 result = match l1, l2 with 
    | [], [] -> result
    | h1 :: t1, h2 :: t2 -> if h1 < h2 then sub1 t1 l2 (h1 :: result) else sub1 l1 t2 (h2 :: result)
    | [], h2 :: t2 ->  sub1 l1 t2 (h2 :: result)
    | h1 :: t1, [] -> sub1 t1 l2 (h1 :: result)
  in 
  List.rev (sub1 l1 l2 [])
      

let slice list = 
  let rec form_pair list (l1, l2) n = match list with
    | [] -> (l1, l2)
    | h :: t when n mod  2 = 0 -> form_pair t (h :: l1, l2) (n + 1)
    | h :: t -> form_pair t (l1, h :: l2) (n + 1) 
  in 
  match (form_pair list ([], []) 0) with | (l1, l2) -> (List.rev l1, List.rev l2)
                                                       


let merge_sort list = 
  let rec sub1 list = match slice list with 
    | l1, l2 when List.length l1 >= 2 || List.length l2 >= 2 ->  interclaseaza (sub1 l1) (sub1 l2)
    | l1, l2 -> interclaseaza l1 l2
  in
  sub1 list;;
                  
    
      



  