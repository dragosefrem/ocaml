(* 8. Scrieți o funcție care ia ca parametri două dicționare de la șiruri l
a șiruri reprezentând funcții parțiale f1 și f2 și returnează dicționarul 
care reprezintă f2 ⚪ f1. *) 

module MyMap = Map.Make(String);;

  
let compound dict1 dict2 = 
  let rec sub l1 dict2 dict3 = match l1 with
    | [] -> dict3 
    | (x,y)  :: t -> try (sub t dict2 (MyMap.add x (MyMap.find y dict2) dict3)) with Not_found -> sub t dict2 dict3
                                                                                                      
  in
  sub (MyMap.bindings dict1) dict2 MyMap.empty;;