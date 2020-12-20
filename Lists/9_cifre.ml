(* 9. Scrieți o funcție care ia o listă de cifre și returnează valoarea 
numărului cu cifrele respective. *)

let  list_to_number list = 
  let rec sub1 list result = 
    match list with
    | [] -> result
    | h :: t -> sub1 t (result * 10 + h)
  in sub1 list 0
;;