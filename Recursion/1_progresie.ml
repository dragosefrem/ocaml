(* Având ca exemplu progresia aritmetică discutată la curs, 
scrieți pentru progresia geometrică: *)

(* a) o funcție recursivă, folosind valori constante pentru primul termen și rație *)

let rec pgb2r2 n = if n = 0 then 2 else 2 * pgb2r2 (n - 1) (* baza = 2, ratia = 2 *)
                                                             
(* b) o funcție care are ca parametri și aceste două valori. Folosiți let ... in 
și o funcție auxiliară cu un parametru *)

let progresie_geometrica baza ratie = 
  let rec aux n = match n with
    | 0 -> baza
    | n -> ratie * aux (n - 1)
  in aux
  

(* c) definiți o funcție pentru progresia de la punctul a) 
dând parametri funcției scrise la punctul b) *)

let pgb3r9 = progresie_geometrica 3 9;;