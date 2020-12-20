  (* 8. Aproximări și limite
Pentru termenii √a+√a+ ...√a și 1/(a+1/(a+1/(... + 1/a))) găsiți relațiile de recurență care exprimă valoarea termenului cu n+1 apariții ale lui a în funcție de termenul cu n apariții. Apoi găsiți la limită valoarea termenilor cu număr infinit de apariții.
Folosiți ca punct de plecare exercițiul rezolvat pentru rădăcina pătrată. *)

(* Relatia de recurenta pentru radical e x_(n+1) = sqrt(a + x_n) si pentru fractie e x_(n+1) = 1 / (a + x_n) *)


let recursive_radical a = 
  let rec aux initial previous = 
    let operation = sqrt(initial +. previous) in
    if abs_float (operation -. previous) < 1e-6 then operation else aux initial operation
  in 
  aux a 0.;;

  let recursive_fraction a = 
    let rec aux initial previous = 
      let operation = 1./.(a +. previous) in
      if abs_float (operation -. previous) < 1e-6 then operation else aux initial operation
    in 
    aux a 0.;;
