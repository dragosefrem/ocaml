let countif l cond = List.fold_left (fun acc e -> if cond e then  acc + 1 else acc) 0 l;;
let sumif l cond = List.fold_right (fun e acc -> if cond e then acc + e else acc) l 0;;
