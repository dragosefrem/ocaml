(* 5. Cifrele unui număr
Un număr e reprezentat uzual în scris ca un șir de cifre în baza 10.
Un șir e o noțiune recursivă (un element, sau un șir urmat de un element).
Putem spune atunci că un număr n e fie o singură cifră, fie ultima cifră (n mod 10) precedată de alt număr 
(n / 10).
Folosind această definiție scrieți funcții recursive care calculează: suma cifrelor unui număr, numărul de 
cifre, produsul lor, cifra maximă / minimă, etc. *)

let rec sum_cif n = if n / 10 = 0 then n else (n mod 10 + sum_cif (n / 10));;
let rec num_cif n = if n / 10 = 0 then 1 else (1 + num_cif (n / 10));;
let rec prod_cif n = if n / 10 = 0 then n else (n mod 10 * prod_cif (n / 10));;
let rec max_cif n = if n / 10 = 0 then n else max (n mod 10) (max_cif (n / 10));;
let rec min_cif n = if n / 10 = 0 then n else min (n mod 10) (min_cif (n / 10));;