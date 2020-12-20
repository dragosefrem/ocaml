(* Program pentru aproximarea radacinii patrate a unui numar - returneaza un numar foarte apropiat de radicalul numarului dat ca parametru *)

let  aprox number  = 
  let rec aux initial_guess previous_guess = 
    let operation = (initial_guess +. number/.initial_guess)/.2. in
    if abs_float (initial_guess -. previous_guess) < 1e-6 then initial_guess else aux operation initial_guess
  in 
  aux 1. 0.;;
  ;;