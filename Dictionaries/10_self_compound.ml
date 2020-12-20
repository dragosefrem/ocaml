(* 10. Fie un dicționar de la șiruri la șiruri reprezentând o funcție parțială f.
a) Scrieți o funcție (având ca parametru un astfel de dicționar) 
   care returnează dicționarul reprezentând f2 = f ⚪ f, și apoi o funcție care ca
    calculează dicționarul pentru fn.
b) Modificați funcția astfel încât să genereze o excepție dacă fn are 
   un punct fix (există x cu fn(x) = x)
c) Aplicați repetat funcția de mai sus pentru a determina dacă funcția 
f are un ciclu (se va genera fie o excepție, fie se ajunge la relația vidă). *)


module StringMap = Map.Make(String)
  
(* a *)

let self_compound_2 f = 
  let compound f1 f2 = StringMap.fold (fun key v acc -> try StringMap.add key (StringMap.find v f1) acc with Not_found -> acc) f2 StringMap.empty in 
  compound f f
let self_compound f n = 
  let compound f1 f2 = StringMap.fold (fun key v acc -> try StringMap.add key (StringMap.find v f1) acc with Not_found -> acc) f2 StringMap.empty in
  let rec sub f1 f2 n = if n > 1 then sub (compound f1 f2) f2 (n - 1) else f1 in
  sub f f n 
  
(* b *)

exception Punct_Fix
let self_compound_warn f n = if StringMap.exists (fun key v -> key = v) (self_compound f n) then raise Punct_Fix else self_compound f n

(* c *)

exception Has_Cicle
exception Relatie_Vida
  
let checkCicle f = 
  let compound f1 f2 = StringMap.fold (fun key v acc -> try StringMap.add key (StringMap.find v f1) acc with Not_found -> acc) f2 StringMap.empty in 
  let rec sub f1 f2 = if StringMap.is_empty f1 then raise Relatie_Vida else if try StringMap.exists (fun key v -> try (key =  StringMap.find v f1) with Not_found -> raise Exit) f2 with Exit -> false then raise Has_Cicle else sub (compound f1 f2) f2 in
  sub f f

;;
    
    