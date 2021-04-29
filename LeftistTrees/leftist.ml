(*
  Drzewa lewicowe
*)

type 'a queue = Node of 'a * int * 'a queue * 'a queue | Null;;
(* Wartosc w nodzie (priorytet), jak daleko jest lisc, lewe poddrzewo, prawe poddrzewo *)
(* Null = brak dalszego poddrzewa *)

(* wyjatek, gdy chcemy usunac pierwszy element z pustej kolejki *)
exception Empty;;

(* Pusta kolejka *)
let empty = Null;;

(* Zwraca dlugosc najkrotszej sciezki do liscia *)
let npl tree =
	match tree with
	| Null -> -1
	| Node(_, g, _, _) -> g;;

(* Laczy dwie kolejki: t1 i t2 *)
let rec join t1 t2 =
  match t1, t2 with
  | Null, Null -> empty
  | Null, _ -> t2
  | _, Null -> t1
  | Node(v1, g1, l1, p1), Node(v2, g2, l2, p2) ->
    if v1 <= v2 then (* sprawdzenie, czy v1 bedzie korzeniem, jesli nie to wywolujemy funkcje, zamieniajac zmienne *)
      let newTree = join p1 t2 in (* tworzenie jednego z poddrzew nowego drzewa, drugim bedzie l1 *)
      if npl l1 < npl newTree then (* sprawdzenie, ktore z poddrzew bedzie lewe, a ktore prawe *)
        Node(v1, (npl newTree + 1), newTree, l1)
      else
        Node(v1, (npl l1 + 1), l1, newTree)
    else
      join t2 t1;; 
	  
(* Dodaje do kolejki tree element elem - laczy dwie kolejki *)
let add elem tree =
  let newTree = Node(elem, 0, Null, Null) in
  join tree newTree;;

(* Usuwa z kolejki element o najwyzszym priorytecie
i zwraca go wraz z kolejka po jego usunieciu *)
let delete_min tree =
  match tree with
  | Null -> raise Empty (* nie mozna usunac elementu z pustej kolejki, wyjatek *)
  | Node(v, _, l, p) -> (v, (join l p));;

(* Sprawdza, czy kolejka jest pusta *)
let is_empty tree =
  match tree with
  | Null -> true
  | _ -> false;;
