(*
 * Zadanie 4. Origami
*)

type point = float * float
type kartka = point -> int

let epsilon = 0.00000001 (* margines bledu obliczen, mozna zmienic na mniejszy *)

(* Tworzy kartke w ksztalcie prostokata, tzn.
   procedure sprawdzajaca, czy dany punkt lezy na niej *)
let prostokat ((xp, yp) : point) ((xq, yq) : point) =
  function ((x, y) : point) ->
    if x >= xp && x <= xq && y >= yp && y <= yq then 1 else 0

(* Tworzy kartke w ksztalcie kola; sprawdza, czy dany
   punkt lezy na niej *)
let kolko ((p, q) : point) (r : float) =
  function ((x, y) : point) ->
    let (u, v) = (x -. p, y -. q) in
    let temp = Float.sqrt (u *. u +. v *. v) in
    if (-1.) *. epsilon -. r <= temp && temp <= r +. epsilon then 1 else 0

(* Procedura skladajaca kartke wzdluz osi wyznaczonej przez dwa rozne punkty
   P1 = (a,b) oraz P2 = (c,d); skladanie przebiega od strony
   prawej do lewej wzgledem wektora P1P2 *)
let zloz ((a, b) : point) ((c, d) : point) (k : kartka) =
  function ((p, q) : point) ->
    let (v1, v2) = (c -. a, d -. b) in (* wektor (a,b)(c,d) *)
    let (p1, q1) = (p -. a, q -. b) in (* wektor (a, b)(p, q) zaczepiony w punkcie (0, 0) *)
    let sign = q1 *. v1 -. p1 *. v2 in (* wspolrzedna Y obroconego puntku *)
    if sign > epsilon then (* punkt lezy po lewej stronie wektora *)
      let mianownik = v1 *. v1 +. v2 *. v2 in (* dlugosc wektora (v1, v2) podniesiona do kwadratu *)
      let temp1 = p1 +. 2. *. v2 *. (q1 *. v1 -. p1 *. v2) /. mianownik and (* wspolrzedna x obrazu punktu (p1, q1) w symetrii osiowej *)
          temp2 = q1 +. 2. *. v1 *. (p1 *. v2 -. q1 *. v1) /. mianownik in (* jak wyzej, wspolrzedna y *)
      k (p, q) + k (a +. temp1, b +. temp2) else
    if sign < (-1.) *. epsilon then (* punkt lezy po prawej stronie osi *)
      0
    else (* punkt lezy na osi, bez zmian *)
      k (p, q)

(* Procedura skladaj wywoluje procedure zloz na danej kartce k
   kolejno z parami punktow bedacych elementami listy ls *)
let skladaj (ls : (point * point) list) (k : kartka) =
  (* funkcja pomocnicza wywolujaca procedure zloz dla pary punktow *)
  let aux kar (x, y) = zloz x y kar in
  List.fold_left aux k ls
