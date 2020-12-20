(* 9. Scrieți o funcție care ia ca parametru un dicționar reprezentând o 
funcție parțială f de la șiruri la șiruri și calculează pentru un șir s n
umărul maxim n pentru care fn(s) e definit, respectiv generează o excepție 
dacă șirul fn(s) e ciclic (definit pentru orice n). *)

module MyMap = Map.Make(String);;
exception Is_ciclic

  
let depth dict s = 
  let rec sub dict s history nr = 
    if List.mem s history then raise Is_ciclic
    else try sub dict (MyMap.find s dict) (s :: history) (nr + 1) with Not_found -> nr 
  in
  sub dict s [] 0;;