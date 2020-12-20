let compare_lists l1 l2 = (
  let rec compare_elements l1 l2 = match l1, l2 with
    | ([], []) -> 0
    | (a, b) when a <> b -> if a > b then 1 else -1
    | (a, b) -> compare_elements (List.tl l1) (List.tl l2)
  in
  if List.length l1 > List.length l2 then 1
  else if List.length l2 > List.length l1 then -1
  else compare_elements l1 l2
)
;;