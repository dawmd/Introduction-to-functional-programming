(* 
   Zadanie 1 - Arytmetyka
*)


type wartosc = Przedzial of float * float | Dopelnienie of float * float;;

(*
   Przedzial(a,b) -> ZAWSZE a <= b
   Dopelnienie(a,b) -> ZAWSZE a <= b
*)

(* KONSTRUKTORY *)

let wartosc_od_do x y = Przedzial (x, y);; (* x <= y *)
let wartosc_dokladna x = Przedzial(x, x);;
let wartosc_dokladnosc x p = (* p > 0 *)
  let temp = x *. (p /. 100.) in
  if x >= 0. then wartosc_od_do (x -. temp) (x +. temp)
  else wartosc_od_do (x +. temp) (x -. temp);;


(* SELEKTORY *)

(* czy w zbiorze x jest liczba y *)
let in_wartosc x y =
  match x with
  | Przedzial(p, q) ->
   (match classify_float p, classify_float q with
   | FP_nan, _ -> false
   | _, FP_nan -> false
   | _, _ -> p <= y && y <= q)
  | Dopelnienie(a, b) ->
   (match classify_float a, classify_float b with
   | FP_nan, _ -> false
   | _, FP_nan -> false
   | _, _ -> y <= a || y >= b);;



(* minimalna wartosc w przedziale x *)
let min_wartosc x =
  match x with
  | Przedzial(p,q) -> 
    (match classify_float p, classify_float q with
    | FP_nan, _ -> nan
    | _, FP_nan -> nan
    | _, _ -> p)
  | Dopelnienie(a,b) ->
    (match classify_float a, classify_float b with
    | FP_nan, _ -> nan
    | _, FP_nan -> nan
    | FP_infinite, FP_infinite ->
      if a = b then neg_infinity
      else nan (* zbior pusty *)
  | FP_infinite, _ -> b
  | _, _ -> neg_infinity);;
 
 
 (* maksymalna wartosc w przedziale x *)
 let max_wartosc x =
  match x with
  | Przedzial(p,q) -> 
    (match classify_float p, classify_float q with
 	| FP_nan, _ -> nan
 	| _, FP_nan -> nan
 	| FP_infinite, FP_infinite ->
 	if p = neg_infinity && q = neg_infinity then neg_infinity
 	 else infinity
 	| _, _ -> q)
  | Dopelnienie(a,b) ->
    (match classify_float a, classify_float b with
	| FP_nan, _ -> nan
	| _, FP_nan -> nan
	| FP_infinite, FP_infinite -> 
	 if a = b then infinity
	 else nan (* zbior pusty *)
	| _, FP_infinite -> a
	| _, _ -> infinity);;

(* srednia wartosc elementow w przedziale x, w przypadku przedzialu (-inf, +inf) nan - nie da sie okreslic *)
let sr_wartosc x =
  let minim = min_wartosc x and maxim = max_wartosc x in
  match classify_float minim, classify_float maxim with
  | FP_nan, _ -> nan
  | _, FP_nan -> nan
  | FP_infinite, FP_infinite -> if minim = maxim then minim else nan
  | _, FP_infinite -> maxim
  | FP_infinite, _ -> minim
  | _, _ -> (maxim -. minim) /. 2. +. minim;; (* kolejnosc jest taka, aby nie wyjsc poza zakres floata *)
 
 
 (* MODYFIKATORY *)
 
(* Wynikiem jest taki zbior c, ze c = {x+y : x \in a, y \in b} *)
let rec plus a b =
   (* sprawdzenie, czy wartosci w przedziale nie sa uszkodzone *)
  let minA = min_wartosc a and maxA = max_wartosc a and minB = min_wartosc b and maxB = max_wartosc b in
  if classify_float minA = FP_nan || classify_float maxA = FP_nan || classify_float minB = FP_nan || classify_float maxB = FP_nan
    then Przedzial(nan, nan) else
  match a, b with
  | Przedzial(_, _), Przedzial(_, _) -> Przedzial((minA +. minB), (maxA +. maxB))
  | Przedzial(_, _), Dopelnienie(p, q) -> if classify_float p = FP_nan || classify_float q = FP_nan then Przedzial(nan, nan) else
    let temp1 = p +. maxA and temp2 = q +. minA in
    if temp1 < temp2 then Dopelnienie(temp1, temp2) else Przedzial(neg_infinity, infinity)
    (* maksymalna wartosc "lewej" czesci to p + maxPrzedzialu, a najmniejsza "prawej" to q + minPrzedzialu.
    Liczby z przedzialu (p+maxA, q+minA) sa nieosiaglne. Jesli jednak "nachodza" na siebie te dwa zbiory,
    to znaczy, ze taka liczba nie istnieje *)	
  | Dopelnienie(_, _), Przedzial(_, _) -> plus b a
  | Dopelnienie(p, q), Dopelnienie(_, _) -> if classify_float p = FP_nan || classify_float q = FP_nan then Przedzial(nan, nan) else
    if p +. maxB < q +. minB then Dopelnienie((p +. maxB), (q +. minB))
    else Przedzial(neg_infinity, infinity);;
    (* jesli zachodzi ta nierownosc, to oznacza, ze maksymalna wartosc "lewego" przedzialu jest mniejsza najmniejszej wartosci "prawego",
    a wiec istnieje taka liczba, ktora znajduje sie pomiedzy tymi wartosciami, a ktorej nie da sie osiagnac.
    W przeciwnym wypadku dostajemy (-inf, +inf) *)
   
 
 (* FUNKCJE POMOCNICZE *)  
 
(* funkcja wyznaczajaca minimalny iloczyn sposrod czterech danych liczb,
pomijajaca wartosci nie bedace liczbami *)
let pomMin lista1 lista2 =
  let rec aux2 el ls acc =
    match ls with
    | [] -> acc
    | h::t -> if classify_float (el *. h) = FP_nan && 0. < acc then aux2 el t 0. else (* przypadek 0. *. +/- inf = 0. *)
      if classify_float (el *. h) = FP_nan then aux2 el t acc else
      if h *. el < acc then aux2 el t (h *. el)
      else aux2 el t acc in
  let rec aux ls1 ls2 acc =
    match ls1 with
    | [] -> acc
    | h::t -> aux t ls2 (aux2 h ls2 acc)
in aux lista1 lista2 infinity;;
  
 
(* analogiczna funkcja maksimum *)
let pomMax lista1 lista2 =
  let rec aux2 el ls acc =
    match ls with
    | [] -> acc
    | h::t -> if classify_float (el *. h) = FP_nan && 0. > acc then aux2 el t 0. else (* 0. *. +/- inf = 0. *)
      if classify_float (el *. h) = FP_nan then aux2 el t acc else
      if h *. el > acc then aux2 el t (h *. el)
      else aux2 el t acc in
  let rec aux ls1 ls2 acc =
    match ls1 with
    | [] -> acc
    | h::t -> aux t ls2 (aux2 h ls2 acc)
in aux lista1 lista2 neg_infinity;;
 
 
(* sumowanie dwoch zbiorow bedacych wynikami czastkowymi procedury razy *)
let zsumujZbiory z1 z2 =
  match z1, z2 with
  | Przedzial(a, b), Przedzial(c, d) ->
    let lewy = min b d and prawy = max a c in
    if lewy >= prawy then Przedzial(neg_infinity, infinity) else Dopelnienie(lewy, prawy)
  | Dopelnienie(p, q), Dopelnienie(r, s) -> let lewy = max p r and prawy = min q s in
    if lewy >= prawy then Przedzial(neg_infinity, infinity) else Dopelnienie(lewy, prawy)
  | _, _ -> z1;; 
 
 
(* ------------- *)
 
(* wynikiem jest zbior c = {x *. y : x \in a, y \in b} *)
let rec razy a b =
  let minA = min_wartosc a and maxA = max_wartosc a and minB = min_wartosc b and maxB = max_wartosc b in
  if classify_float minA = FP_nan || classify_float maxA = FP_nan || classify_float minB = FP_nan || classify_float maxB = FP_nan
    then Przedzial(nan, nan) else
  match a, b with
  | Przedzial(_, _), Przedzial(_, _) ->
    if (minA = 0. && maxA = 0.) || (minB = 0. && maxB = 0.) then Przedzial(0., 0.) else
    let minimalna = pomMin [minA;maxA] [minB;maxB] and maksymalna = pomMax [minA;maxA] [minB;maxB] in
    Przedzial(minimalna, maksymalna) (* prosty dowod z analizy matematycznej *)
  | Przedzial(_, _), Dopelnienie(_, _) -> razy b a
    (* rozbijamy dopelnienie na dwa rozlaczne zbiory, obliczamy dla nich wyniki i sumujemy *)
  | Dopelnienie(p, q), Przedzial(_, _) -> 
    if (minB = 0. && maxB = 0.) then Przedzial(0.,0.) else		
    (* jesli Przedzial nie jest singletonem zera, to dostaniemy ostatecznie albo pewne dopelnienie przedzialu, albo (-inf, +inf) *)
    let zbior1 = razy (Przedzial(neg_infinity, p)) b and zbior2 = razy (Przedzial(q, infinity)) b in
    zsumujZbiory zbior1 zbior2
  | Dopelnienie(p, q), Dopelnienie(u, v) ->
    let zbior1 = razy a (Przedzial(neg_infinity, u)) and zbior2 = razy a (Przedzial(v, infinity)) in
    (* rozbijamy zbior b na dwa rozlaczne podzbiory i obliczamy wyniki czastkowe, korzystajac z poprzedniego przypadku *)
    match zbior1, zbior2 with
    | Przedzial(_, _), _ -> zbior1 (* otrzymamy przedzial tylko, gdy (-inf, inf), wiec sumujac dostaniemy i tak (-inf, inf) *)
    | _, Przedzial(_, _) -> zbior2 (* podobnie jak wyzej *)
    | _, _ -> zsumujZbiory zbior1 zbior2;; (* w przeciwnym przypadku dostalismy dwa zbiory bedace dopelnieniami *)
 
 
(* kazdy element w b jest mnozony razy -1 i nastepnie sumujemy takie zbiory *)
let minus a b =
  plus a (razy b (Przedzial(-1.,-1.)));;
 
 
(* Dzielenie polegajace na obliczeniu przedzialu/dopelnienia {1}/b, a nastepnie pomnozeniu przez zbior a *)
let podzielic a b =
  let rec aux b =
    let minim = min_wartosc b and maxim = max_wartosc b in
    match b with
    | Przedzial(_, _) ->
      (* w razie "zepsutego" przedzialu *)
      if classify_float minim = FP_nan || classify_float maxim = FP_nan then Przedzial(nan, nan) else
   	  if minim = neg_infinity && maxim = infinity then Przedzial(neg_infinity, infinity) else
   	  if minim > 0. then Przedzial((1. /. maxim), (1. /. minim)) else
   	  if minim = 0. && maxim = 0. then Dopelnienie(neg_infinity, infinity) else
 	  if minim = 0. then Przedzial((1. /. maxim), infinity) else
 	  if minim < 0. && maxim > 0. then Dopelnienie((1. /. minim), (1. /. maxim)) else
 	  if minim < 0. && maxim = 0. then Przedzial(neg_infinity, (1. /. minim))
 	  else Przedzial((1. /. maxim), (1. /. minim))
    | Dopelnienie(p, q) -> 
      (* to samo - gdyby dopelnienie bylo "zepsute" i mialo nan *)
      if classify_float p = FP_nan || classify_float q = FP_nan then Przedzial(nan, nan) else
      if p = neg_infinity && q = infinity then Przedzial(neg_infinity, infinity) else
	  if p > 0. then Dopelnienie((1. /. q), (1. /. p)) else
	  if p = 0. && q = 0. then Przedzial(neg_infinity, infinity) else
	  if p = 0. then Przedzial(neg_infinity, (1. /. q)) else
	  if p < 0. && q > 0. then Przedzial((1. /. p), (1. /. q)) else
	  if p < 0. && q = 0. then Przedzial((1. /. p), infinity)
	  else Dopelnienie((1. /. q), (1. /. p))
in razy a (aux b);;
(* wszystkie powyzsze warunki i wyniki wynikaja z analizy funkcji f(x) = 1/x *)
