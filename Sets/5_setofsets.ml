(* 5. Scrieți o funcție care ia o mulțime de mulțimi (de exemplu, de șiruri), 
și returnează reuniunea (variantă: intersectia) mulțimilor. *) 


module Int = struct
  type t = int
  let compare = compare
end

module IntSet = Set.Make(Int)
module IntOfIntSet = Set.Make(IntSet)
    
let unionOfSets m = 
  let rec sub m union = match IntOfIntSet.elements m with
    | [] -> union
    | h :: t -> sub (IntOfIntSet.of_list t) (IntSet.union h union)
  in
  sub m (IntSet.empty);;

let interOfSets m = 
  let rec sub m union = match IntOfIntSet.elements m with
    | [] -> union
    | h :: t -> sub (IntOfIntSet.of_list t) (IntSet.inter h union)
  in
  sub m (match IntOfIntSet.elements m with [] -> IntSet.empty | h :: t -> h);;
  
  
    
    
