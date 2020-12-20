let filter cond l = List.fold_right (fun e acc -> if cond e then e :: acc else acc) l [];; 

let rec exists cond l = match l with
  | [] -> false
  | h :: t when cond h -> true
  | h :: t -> exists cond t
                
let mem n l = exists (fun x -> x = n) l
;;