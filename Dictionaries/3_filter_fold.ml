(* 3. Implementați cu ajutorul lui fold din modulul Map funcția filter 
care creează un nou dicționar doar cu perechile din dicționarul dat 
care satisfac o funcție dată. *)

module MyMap = Map.Make(String);;

let filter f map = MyMap.fold (fun key valoare acc -> if (f key valoare) then MyMap.add key valoare acc else acc) map MyMap.empty