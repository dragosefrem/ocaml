(* 8. Scrieți o funcție care ia ca parametru o mulțime s și un număr k și returnează lista (sau mulțimea) tuturor submulțimilor lui s cu k elemente. *) 


module Int = struct
  type t = int
  let compare = compare
end 

module IntSet = Set.Make(Int)
module IntOfIntSet = Set.Make(IntSet);;

let submultime m k = 
  let rec recursive_fold m k accr = 
    IntSet.fold (fun e acc -> if k > 1 then IntOfIntSet.union (recursive_fold (IntSet.remove e m) (k - 1) (IntSet.add e accr)) acc else IntOfIntSet.add (IntSet.add e accr) acc) m IntOfIntSet.empty 
  in
  if k > 0 then recursive_fold m k IntSet.empty else IntOfIntSet.empty
  