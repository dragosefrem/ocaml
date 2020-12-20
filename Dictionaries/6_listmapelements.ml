(* 6. Scrieți o funcție care primește un dicționar de la șiruri la întregi 
și o listă de șiruri și returnează mulțimea tuturor valorilor din dicționar 
care corespund șirurilor din listă. *)

module Int = struct
  type t = int
  let compare = compare
end

module IntSet = Set.Make(Int)
module StringMap = Map.Make(String)
    
let listMapElements map list = List.fold_left (fun acc e -> try (IntSet.add (StringMap.find e map) acc) with Not_found -> acc) IntSet.empty list