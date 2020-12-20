(* 6. Scrieți o funcție care returnează mulțimea cifrelor unui număr. 
Scrieți apoi altă funcție care ia o mulțime de numere și returnează 
reuniunea/intersecția dintre mulțimile cifrelor lor. *) 

module Int = struct
  type t = int
  let compare = compare
end

module IntSet = Set.Make(Int)
    
let setOfDigits number = if number = 0 then IntSet.add 0 IntSet.empty
  else
    (let rec aux number = match number with
        | 0 -> IntSet.empty
        | _ -> IntSet.add (number mod 10) (aux (number / 10)) in aux number)
    
let union_inter_mulcif multime = IntSet.fold 
    (fun el acc -> match acc with (a, b) -> (IntSet.union (setOfDigits el) a, IntSet.inter (setOfDigits el) b)) 
    multime (IntSet.empty, (match IntSet.elements multime with [] -> IntSet.empty |h :: t -> setOfDigits h))
  
           
;;