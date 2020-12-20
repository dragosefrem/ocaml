(* Exercițiul 4: valori distincte
Scrieți o funcție cu trei parametri (de același tip oarecare), care returnează 
   câte valori distincte există între argumentele primite (unul, două sau trei) 
   și tipărește, după caz, un mesaj: "toate argumentele sunt distincte/egale" 
   sau "argumentele 1 și 2 (resp. 2 și 3, sau 1 și 3) sunt egale". Evitați pe cât 
   posibil duplicarea de cod: pentru porțiuni de cod similare, 
   creați (și apelați) o funcție care conține partea comună și are ca pa
   rametri valorile care diferă. *)

let print_equal a b = Printf.printf "Argumentele %d si %d sunt egale" a b

let comp a b c = 
  if a = b then (if a = c then (Printf.printf "Toate argumentele sunt egale"; 0)
                 else (print_equal 1 2; 2))
  else if a = c then (print_equal 1 3; 2)
  else if b = c then (print_equal 2 3; 2)
  else (Printf.printf "Toate argumentele sunt distincte"; 3);;