(* 8. Scrieți o funcție care ia ca parametru o mulțime s și un număr k și returnează lista (sau mulțimea) tuturor submulțimilor lui s cu k elemente. *) 


module Int = struct
  type t = int * int
  let compare = compare
end 

module IntSet = Set.Make(Int)
    
let prim number = 
  let rec primeDivisors number listOfDivisors p = 
    if number = 1 then listOfDivisors
    else if number mod p = 0 then primeDivisors (number / p) (p :: listOfDivisors) p
    else primeDivisors (number) listOfDivisors (p + 1)
  in
  let divisorList =  List.rev (primeDivisors number [] 2) in 
  List.fold_left (fun acc e -> IntSet.add (e, List.length(List.filter (fun x -> x = e) divisorList)) acc) IntSet.empty divisorList;;



  
  