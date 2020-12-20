(*

3. a) Implementați funcția List.nth care returnează al n-lea element dintr-o listă.
Observați întâi în interpretor comportamentul funcției standard pentru valori ale lui n invalide (negative) 
sau prea mari (mai mari decât lungimea listei). Puteți produce excepția Invalid_argument mesaj apelând 
funcția invalid_arg mesaj, și excepția Failure mesaj apelând funcția failwith mesaj.
b) Implementați o funcție firstn care returnează o listă cu primele n elemente dintr-o listă dată.

*)

let rec nth l = function
  | n when n < 0 -> invalid_arg "Argument negativ"
  | n when n >= (List.length l) -> failwith "Argument mai mare decat lungimea listei"
  | 0 -> List.hd l;
  | n -> nth (List.tl l) (n - 1)
           

let rec firstn l = function
  | n when n < 0 -> invalid_arg "Argument negativ"
  | n when n >= (List.length l) -> failwith "Argument mai mare decat lungimea listei"
  | 0 -> [List.hd l]
  | n -> (List.hd l) :: firstn (List.tl l) (n - 1)
;;
