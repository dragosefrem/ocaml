
  (* 9. Serii
Scrieți o funcție care calculează valoarea lui ex folosind dezvoltarea în serie Taylor: ex = 1 + x/1! + x2/2! + ... + xn/n! + ... , 
calculând până când termenul curent devine suficient de mic. Evitați recalcularea inutilă a puterii și factorialului, 
găsind o relație de calcul pentru termenul curent al seriei pornind  de la cel anterior, pe care îl transmiteți ca parametru la funcția pe care o scrieți. *)

let calculate_euler exp = 
  let rec aux act_el pr_sum act_sum n =
    if abs_float (act_sum -. pr_sum) < 1e-6 then act_sum else aux (act_el *. exp /. n) act_sum (act_sum +. act_el) (n +. 1.)
  in
  aux 1. 1. 0. 1.;; 