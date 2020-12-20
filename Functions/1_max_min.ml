(* Exercițiul 1: minim/maxim (discutat la curs) Scrieți o funcție care returnează minimul/maximul a 
trei valori date ca parametri. Folosiți funcțiile predefinite min respectiv max care funcționează cu 
orice valori de același tip. Remarcați tipul funcției scrise și verificați că funcționează și cu întregi
 și cu reali (și chiar cu șiruri), însă nu cu un amestec. *)

 let maxim a b c = max a (max b c)
 let minim a b c = min a (min b c);;

 

