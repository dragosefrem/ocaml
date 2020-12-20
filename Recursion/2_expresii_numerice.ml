type expr = I of int
 | Add of expr * expr
 | Sub of expr * expr
 | Mul of expr * expr
 | Div of expr * expr

;;

 let rec calc exp = match exp with
 | I k -> k
 | Add (exp1, exp2) -> calc exp1 + calc exp2
 | Sub (exp1, exp2) -> calc exp1 - calc exp2
 | Mul (exp1, exp2) -> calc exp1 * calc exp2
 | Div (exp1, exp2) -> calc exp1 / calc exp2


 ;;

 let expr1 = Add(Mul (I 2, Sub (I 3, I 8)), I 4);;
 calc expr1;;
 let expr2 = Sub(Add (I 2 , I 4), Mul (I 5, I 3) );;
 calc expr2;;
 