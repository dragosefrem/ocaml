(* 2. Asemănător cu funcția set_of_intlist, scrieți o funcție care ia o 
listă de perechi (de tip precizat) și returnează mulțimea elementelor de 
pe prima poziție din fiecare pereche (variante: a doua poziție; ambele poziții, 
dacă sunt de același tip). *)

module Int = struct
  type t = int
  let compare = compare
end

module IntSet = Set.Make(Int)
    
let pairToSet1 listOfPairs = 
  let rec sub listOfPairs set = match listOfPairs with
    | [] -> set 
    | (a, b) :: t -> sub t (IntSet.add a set)
  in
  sub listOfPairs (IntSet.empty)
    
let pairToSet2 listOfPairs = 
  let rec sub listOfPairs set = match listOfPairs with
    | [] -> set 
    | (a, b) :: t -> sub t (IntSet.add b set)
  in
  sub listOfPairs (IntSet.empty)
    
let pairToSet3 (listOfPairs: ('a * 'b) list) = 
  let rec sub listOfPairs set = match listOfPairs with
    | [] -> set 
    | (a, b) :: t -> sub t (IntSet.add a (IntSet.add b set))
  in
  sub listOfPairs (IntSet.empty)
;;

    
    
