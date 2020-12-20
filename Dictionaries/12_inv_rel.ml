(* 12. a) Fiind dată o funcție reprezentată ca dicționar, 
calculați inversa functiei dacă există. În caz contrar semnalați o 
  excepție: failwith "functie neinversabila". *)

module StringMap = Map.Make(String)
module StringSet = Set.Make(String)

let inverse f = 
  let inv = StringMap.fold (fun key v acc -> StringMap.add v key acc) f StringMap.empty in
  if List.length (StringMap.bindings inv) = List.length (StringMap.bindings f) then inv else failwith "functie neinversabila"
  

(* b) Fiind dată o relație R pe S implementată ca dicționar de la S la P(S) 
(mulțimea părților lui S), scrieți o funcție care returnează inversa relației,
R^-1, reprezentată în același mod. *) 

let inv_relatie rel = StringMap.fold (fun key v acc -> 
    
    let rec sub v newMap  = match StringSet.elements v with 
      | [] -> newMap
      | h :: t -> sub (StringSet.of_list t) (try (StringMap.add h (StringSet.add key (StringMap.find h newMap)) newMap) with Not_found -> (StringMap.add h (StringSet.add key StringSet.empty) newMap))
    in
    sub v acc
       
  )rel StringMap.empty
    
    