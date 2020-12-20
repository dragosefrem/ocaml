(* 11. a) Fiind dată o funcție reprezentată ca dicționar, determinați 
dacă funcția e o involuție, adică dacă e inversabilă și f^-1 = f. *) 

module StringMap = Map.Make(String)
module StringSet = Set.Make(String)

let checkInvolution f = 
  let inverse f = StringMap.fold (fun key v acc -> StringMap.add v key acc) f StringMap.empty in 
  StringMap.bindings f = StringMap.bindings (inverse f) && List.length (StringMap.bindings f) = List.length (StringMap.bindings (inverse f))
  

(* b) Fiind dată o relație pe S implementată ca dicționar de la S la P(S) 
(mulțimea părților lui S), scrieți o funcție care determină dacă relația e 
simetrică. *)

let relatie_simetrica relatie = try (StringMap.fold (fun key v acc -> 
    let rec sub1 mul t = 
      match StringSet.elements mul with
      | [] -> t
      | h :: tail -> sub1 (StringSet.of_list tail) (match StringSet.find key (StringMap.find h relatie) with _ -> true)
    in
    sub1 v true
  ) relatie true) with Not_found -> false 
                                   
;;

