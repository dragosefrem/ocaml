(* 5. Implementați cu ajutorul lui fold funcția standard map care construiește un 
dicționar în care toate valorile au fost transformate folosind o funcție dată 
ca parametru. *)

module MyMap = Map.Make(String);;

let map f dict = MyMap.fold (fun key v acc -> MyMap.add key (f v) acc) dict MyMap.empty 