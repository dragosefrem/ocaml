let rec random_list n b = match n with
  | 0 -> []
  | _ -> Random.int b :: random_list (n - 1) b
           
;;

