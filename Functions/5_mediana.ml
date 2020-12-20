(* Exercițiul 5: Mediana
Scrieți o funcție care calculează mediana a trei valori (valoarea aflată între celelalte două).
Încercați să scrieți cod cât mai simplu, și să nu-l repetați. Puteți folosi o funcție auxiliară care calculează
mediana a trei numere, pentru care știm că primul e mai mic sau egal decât al doilea. Sau puteți încerca
să compuneți doar funcțiile standard max/min de două elemente (expresia trebuie să fie oarecum
simetrică). Care din variante necesită mai puține comparații? *)

let mediana a b c = a + b + c - max a (max b c) - min a (min b c) (* aceasta varianta necesita 4 comparatii *)

(* Test zone *)

let t0 = mediana 1 2 3 (* rezultat asteptat: 2 *)
let t1 = mediana 1 3 2 (* rezultat asteptat: 2 *)
let t2 = mediana 3 1 2 (* rezultat asteptat: 2 *)
let t3 = mediana 3 2 1 (* rezultat asteptat: 2 *)
let t4 = mediana 2 3 1 (* rezultat asteptat: 2 *)
let t5 = mediana 2 1 3 (* rezultat asteptat: 2 *)

;;
 