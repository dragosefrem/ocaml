(* 3. Tipărire indentată - Scrieți o funcție care afișează un arbore de întregi 
în a) preordine; b) inordine, câte un nod pe linie, precedând valoarea din nod 
cu un număr de spații egal cu dublul adâncimii la care se află (câte două spații 
pentru fiecare nivel). *) 

type 'a tree = T of 'a * 'a tree list 
                                
let afiseaza_preordine tree = 
  let rec print_spaces nr = if nr > 0 then (print_string " "; print_spaces (nr - 1)) else () in
  let rec sub tree nr = match tree with 
    | T(r, l) -> print_spaces nr; Printf.printf "%d\n" r; List.iter (fun e -> sub e (nr + 2)) l
  in
  sub tree 0
    
let afiseaza_inordine tree = 
  let rec print_spaces nr = if nr > 0 then (print_string " "; print_spaces (nr - 1)) else () in
  let rec sub tree nr = match tree with
    | T(r, l) ->  (try sub (List.hd l) (nr + 2) with _ -> ());print_spaces nr; Printf.printf "%d\n" r; try (List.iter (fun e -> sub e (nr + 2)) (List.tl l)) with _ -> () in
  sub tree 0
;;