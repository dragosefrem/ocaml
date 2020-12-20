(* 2. Asemănător cu funcția fromto (din notițele de curs) care generează lista numerelor întregi dintr-un 
interval dat, scrieți o funcție care creează lista tuturor întregilor dintr-un interval dat, divizibili cu 
  o valoare dată d.
                    Indicație: Găsiți cel mai mare număr divizibil din interval, și continuați pas cup as. *)

let f e1 e2 d =
  let rec create_list e1 e2 = 
    if e2 > e1 then e2 :: create_list e1 (e2 - 1) else [e1]
  in 
  List.filter (fun x -> x mod d = 0) (List.rev(create_list e1 e2))

;;