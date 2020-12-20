let slice list = 
  let rec form_pair list (l1, l2) n = match list with
    | [] -> (l1, l2)
    | h :: t when n mod  2 = 0 -> form_pair t (h :: l1, l2) (n + 1)
    | h :: t -> form_pair t (l1, h :: l2) (n + 1) 
  in 
  match (form_pair list ([], []) 0) with | (l1, l2) -> (List.rev l1, List.rev l2)
;;
    