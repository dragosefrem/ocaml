(* 7. Scrieți o funcție care primește o funcție și un dicționar și returnează maximul valorilor funcției 
pentru toate intrările dicționarului, sau generează excepția Not_found pentru un dicționar vid.
Funcția-parametru are ca argumente cheia și valoarea unei intrări, și poate returna valori arbitrare. 
Folosiți fold pentru parcurgere, și max (definită implicit pentru orice tip) pentru a compara valorile 
returnate de funcția parametru. *)

let max_val f dict = if MyMap.is_empty dict then raise Not_found
else MyMap.fold (fun key v acc -> max (f key v) acc) dict min_int;;

 ;;