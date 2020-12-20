let partition f l = 
  let rec partition1 l l1 l2 = 
    match l with
    | [] -> (List.rev l1, List.rev l2)
    | h :: t when f h -> partition1 t (h :: l1) l2
    | h :: t -> partition1 t l1 (h :: l2) 
  in
  partition1 l [] [];;           
;;
