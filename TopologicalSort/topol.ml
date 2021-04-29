(*
 * Sortowanie topologiczne
 *)

open PMap;;

(*
 * Mapa (graf) uzyta w rozwiazaniu sklada sie z par
 * postaci ('a list * int). Dla dowolnego klucza el,
 * pierwszy element pary to wierzcholki grafu, do ktorych prowadzi
 * krawedz z el; drugi element to liczba krawedzi wchodzacych
 * do wierzcholka el. 
 *)

exception Cykliczne (* wyjatek rzucany w przypadku grafu z cyklem *)


(* zwieksza liczbe krawedzi wchodzacych do wierzcholka el w mapie mapa *)
let zwieksz mapa el =
  let (ls, licz) =
    try find el mapa with
    (* w przypadku, gdy danego wierzcholka nie bylo wczesniej w grafie *)
    | Not_found -> ([], 0) in
  add el (ls, licz+1) mapa


(* dodaje wierzcholki z listy lista oraz wierzcholek el do grafu *)
let dodaj mapa (el, lista) =
  let (ls, licz) = try find el mapa with
  | Not_found -> ([], 0) in
  (* aby zaoszczedzic na czasie w przypadku pustej listy *)
  let temp = if ls = [] then lista else lista@ls in
  List.fold_left zwieksz (add el (temp, licz) mapa) lista


(* tworzy graf w oparciu o liste lista *)
let zbuduj_graf lista = List.fold_left dodaj empty lista


(* WLASCIWA CZESC PO ZBUDOWANIU GRAFU *)


(* sortowanie topologiczne *)
let topol lista =
  (* tworzymy kolejke q *)
  let q = Queue.create () in
  (* zmniejsza liczbe krawedzi wchodzacych do danego wierzcholka
     i w przypadku, gdy liczba ta jest rowna 0, dodaje do kolejki q *)
  let zmniejsz mapa el =
    let (ls, licz) = try find el mapa with
    | Not_found -> assert(false) in (* w przypadku nieoczkiwanego bledu *)
    if licz = 1 then
      Queue.add el q;
    add el (ls, licz-1) mapa in
  (* tworzenie grafu m w oparciu o liste lista *)
  let m = zbuduj_graf lista in
  (* usuniecie z grafu mapa wierzcholka o zerowej liczbie krawedzi wchodzacych,
     acc to list wynikowa *)
  let rec aux mapa acc =
    if is_empty mapa then acc else (* pusty graf *)
    (* jesli kolejka jest pusta, ale graf nie, to ma on cykl *)
    if Queue.is_empty q then raise Cykliczne else
    let temp = Queue.pop q in
    (* zmniejszamy liczbe krawedzi wchodzacych do wierzcholkow, do ktorych prowadzi
       krawedz z wierzcholka temp oraz ewentualnie dodajemy pewne z nich do kolejki q *)
    let new_map = List.fold_left zmniejsz mapa (fst (find temp mapa)) in
    (* usuwamy wierzcholek temp z grafu *)
    aux (remove temp new_map) (temp::acc)
  in
  (* dodajemy wierzcholki o zerowej liczbie krawedzi wchodzacych do kolejki q *)
  iter (fun key (_, licznik) -> if licznik = 0 then Queue.add key q) m;
  List.rev (aux m [])
