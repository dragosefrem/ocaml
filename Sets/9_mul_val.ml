(* 9. Scrieți o funcție care ia ca parametru o funcție 
(de doi întregi cu rezultat întreg) și două mulțimi de întregi A și B și 
returnează mulțimea valorilor f a b cu a ∈ A și b ∈ B. Adaptați parcurgerile f
ăcute pentru produsul cartezian. *)

module Int = struct
  type t = int
  let compare = compare
end

module IntSet = Set.Make(Int)

let mul_val f mul1 mul2 = IntSet.fold (fun e1 acc1 -> IntSet.union (IntSet.fold (fun e2 acc2 -> IntSet.add (f e1 e2) acc2) mul2 IntSet.empty) acc1) mul1 IntSet.empty
    
;;