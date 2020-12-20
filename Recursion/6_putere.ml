let rec putere a k = if k = 0 then 1 else a * putere a (k - 1)

let rec min_putere a p k = 
  if a mod p = 0 then 0 
  else if (putere a k) mod p = 1 then k 
  else min_putere a p (k + 1)

  ;;