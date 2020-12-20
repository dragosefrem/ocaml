(* Flatten primește ca parametru o listă de liste și returneză lista care conține toate elementele din subliste: [[4;5];[7;8;9];[];[0;2;1]] = [4;5;7;8;9;0;2;1]. *)

let flatten list = 
  let rec add_elements l result = match l with
    | [] -> result
    | h :: t -> add_elements t (h :: result)
  in
  let rec elements list result = match list with
    | [] -> result
    | h :: t -> elements t (add_elements h result)
  in 
  List.rev (elements list [])

;;
    