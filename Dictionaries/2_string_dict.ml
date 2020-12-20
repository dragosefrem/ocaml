(* 2. Scrieți o funcție care ia o listă de șiruri de caractere și 
creează un dicționar în care fiecare șir e asociat cu numărul 
aparițiilor din listă *)

module StringMap = Map.Make(String)
    
let string_nr listOfStrings = 
  let rec sub listOfStrings resultMap =  match listOfStrings with
    | [] -> resultMap
    | h :: t when StringMap.mem h resultMap -> sub t resultMap
    | h :: t -> sub t (StringMap.add h (List.length (List.filter (fun x -> x = h) listOfStrings)) resultMap)
    
  in
  sub listOfStrings StringMap.empty
    
;;
  