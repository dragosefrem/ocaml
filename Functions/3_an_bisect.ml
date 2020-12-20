(* Exercițiul 3: an bisect Scrieți o funcție care determină dacă un an (întreg) dat ca parametru e bisect,
returnând un boolean. Dacă un an e bisect sau nu se poate determina după următoarele reguli (va trebui
sa le reformulați sau reordonați pentru a scrie funcția):
a) un an divizibil la 4 e bisect, altfel nu
b) prin excepție de la a), anii divizibili cu 100 nu sunt bisecți
c) prin excepție de la b), anii divizibili cu 400 sunt bisecți *)

let bisect an = 
  if an mod 4 = 0 then 
    ( if an mod 400 = 0 then true
      else if an mod 100 = 0 then false 
      else true
    )
  else false

(* Test zone *)

let t0 = bisect 400 (* rezultat asteptat: true *)
let t1 = bisect 404 (* rezultat asteptat: true *)
let t2 = bisect 300 (* reultat asteptat: false *)
let t3 = bisect 305 (* rezultat asteptat: false *)

;;