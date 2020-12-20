(* 4. Implementați funcția standard partition care ia ca 
parametri o funcție booleană f și o mulțime s și returnează o 
pereche de mulțimi, cu elementele din s care satisfac, respectiv 
nu satisfac funcția f. *)

module Int = struct
  type t = int
  let compare = compare
end

module IntSet = Set.Make(Int)
    
let partition cond myIntSet = 
  IntSet.fold (fun el acc -> match acc with 
      | (m1, m2) when cond el -> (IntSet.add el m1, m2) 
      | (m1, m2) -> (m1, IntSet.add el m2)
    ) myIntSet (IntSet.empty, IntSet.empty)
    
;;