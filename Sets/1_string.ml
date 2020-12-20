(*  Scrieți o funcție care ia ca parametru o mulțime de șiruri de caractere 
și o tipărește, folosind iteratorul S.iter pentru a parcurge elementele. 
Afișați mulțimea pe o linie, între acolade { } și cu virgulă între elemente. *)

module StringSet = Set.Make(String);;

let print m = Printf.printf "{"; StringSet.iter (fun s -> Printf.printf "%s, " s) m; Printf.printf "\b\b}";

;;