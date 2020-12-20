(* 1. Scrieți o funcție care ia o listă de asociere cu perechi de tip M
(șir, întreg) și creează un dicționar în care fiecare șir e asociat cu 
suma tuturor valorilor cu care e asociat în listă. *)


module MyMap = Map.Make(String);;


let dict_sum l = 
  let rec sub l map = match l with
    | [] -> map
    | h :: t -> match h with (a,b) 
      -> 
        (
          let k = MyMap.bindings(MyMap.filter (fun key x -> key = a) map) in
          match k with | [] -> sub t (MyMap.add a b map)
                       | [(x,y)] -> sub t (MyMap.add x (b+y) map)
                       | _ -> assert false (* pentru a ignora warning-ul 'pattern matching is not exhaustive' *)
        ) 
  in
  sub l MyMap.empty
;;
      
