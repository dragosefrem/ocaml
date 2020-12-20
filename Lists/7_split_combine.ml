let split list = 
  let rec create_pair list pair = match list, pair with
    | [], pair -> pair
    | (a, b) :: t, (l1, l2) -> create_pair t (a :: l1, b :: l2) 
  in 
  let reverse_pair_list pair = match pair with 
    | (a , b) -> (List.rev a, List.rev b) 
  in
  reverse_pair_list (create_pair list ([], []))
    
    

let combine l1 l2 = 
  let rec create_list l1 l2 list = match l1, l2, list with 
    | [], [], list -> list 
    | h1 :: t1, h2 :: t2, list ->  create_list t1 t2 ((h1, h2) :: list) 
    | _, _, _ -> failwith "Listele date ca parametru nu au lungimi egale"
  in 
  List.rev (create_list l1 l2 []);;
    