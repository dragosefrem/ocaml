(* 3. Implementați funcția standard filter care ia ca parametri o 
funcție booleană (condiție, predicat) f și o mulțime s și returnează 
mulțimea elementelor din s care satisfac funcția f. *)

module Int = struct
  type t = int
  let compare = compare
end

module IntSet = Set.Make(Int)
    
let filter cond myIntSet = IntSet.fold (fun el acc -> if cond el then IntSet.add el acc else acc) myIntSet IntSet.empty;;