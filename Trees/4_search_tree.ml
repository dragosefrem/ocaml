 (* 4. Arbore binar de căutare - Scrieți o funcție care determină dacă un arbore este un 
 arbore binar de căutare: pentru fiecare nod, subarborele stâng conține doar valori mai mici, 
 iar cel drept, doar valori mai mari. *)


 let rec search_tree = function
  | Nil -> true
  | BT(s, r, d) -> (search_tree s) && (search_tree d) && ((r > (match s with BT(s1,r1,d1) -> r1 | Nil -> min_int)) && (r < (match d with BT(s2,r2,d2) -> r2 | Nil -> max_int)))

;;