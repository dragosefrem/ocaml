(* 4. Pentru tipurile colecție (liste, mulțimi, dicționare) e util să avem funcții care ne spun dacă există 
un element care satisface o anume condiție, respectiv dacă toate elementele satisfac condiția.
Implementați funcțiile exists și for_all pentru dicționare, folosind fold. Ele iau ca parametru o 
funcție booleană de cheie și valoare (care exprimă condiția) și dicționarul în care se face căutarea. 
(Ele există ca funcții standard, deci puteți vedea tipul lor în documentație). *)

let exists cond dict = try (MyMap.fold (fun key v acc -> if cond key v then raise Exit else acc) dict false) with Exit -> true
let for_all cond dict = try (MyMap.fold (fun key v acc -> if not (cond key v) then raise Exit else acc) dict true) with Exit -> false

;;
