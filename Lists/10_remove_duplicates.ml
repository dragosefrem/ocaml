let rec remove_duplicates = function
  | [] -> []
  | e1 :: e2 :: t when e1 = e2 -> remove_duplicates (e2 :: t)
  | e1 :: t -> e1 :: remove_duplicates t