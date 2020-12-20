(* Exercițiul 2: ecuația de gradul 2 Scrieți o funcție care ia ca parametri trei întregi a, b, c și tipărește soluțiile 
ecuației de gradul doi ax2+bx+c=0, sau un mesaj dacă nu există soluții reale. Folosiți funcția predefinită sqrt : float 
-> float pentru rădăcina pătrată și nu uitați conversiile de la întreg la real unde sunt necesare. Folosiți secvențierea 
când trebuie tipărite două soluții. *)

let patratica a b c = 
  let delta = (float_of_int b) *. (float_of_int b) -. 4. *. (float_of_int a) *. (float_of_int c) in
  if delta < 0. then Printf.printf "Ecuatia nu are solutii reale"
  else if delta = 0. then Printf.printf "x1 = x2 = %f" (-.(float_of_int b) /. (2.*.(float_of_int a)))
  else if delta > 0. then (Printf.printf "x1 = %f " ((-.(float_of_int b) +. sqrt(delta))/.(2.*.(float_of_int a))); Printf.printf "x2 = %f " ((-.(float_of_int b) -. sqrt(delta))/.(2.*.(float_of_int a))) )
;;