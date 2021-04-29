(*
 * Zadanie Przelewanka
 *
 *)

(* mapa odwiedzonych stanow wody w szklankach*)
let g = Hashtbl.create 10000;;

(* kolejka par (stan wody w szklankach, liczba wymaganych krokow) *)
let q = Queue.create ();;



(* oblicza, ile co najmniej razy trzeba przelac wode miedzy szklankami,
   aby otrzymac wynik; jesli nie jest to mozliwe, zwraca -1 *)
let przelewaj x y =
  let len = Array.length x in
  let loop = ref true in (* czy kontynuowac petle *)
  let wynik = ref (-1) in

  (* stan poczatkowy *)
  Queue.add (Array.make len 0, 0) q;

  (* sprawdza, czy znaleziono wynik; dodaje do kolejki q
     jeszcze nie sprawdzone stany i oznacza je jako
     przejrzane w mapie g *)
  let sprawdz stan distance =
    if not (Hashtbl.mem g stan) then
      begin
        if stan = y then
          begin
            loop := false;
            wynik := distance;
          end;
        Hashtbl.add g stan true;
        Queue.add (stan, distance) q;
      end in

  (* dolewanie/przelewanie wody w szklankach *)
  while !loop && (not (Queue.is_empty q)) do
    let (el, dist) = Queue.pop q in

    for i = 0 to len - 1 do
      (* wypelnianie i-tej szklanki woda *)
      if el.(i) < x.(i) then
        begin
          let tmp = Array.copy el in
          tmp.(i) <- x.(i);
          sprawdz tmp (dist+1);
        end;

      (* wylewanie wody z i-tej szklanki *)
      if el.(i) <> 0 then
        begin
          let tmp = Array.copy el in
          tmp.(i) <- 0;
          sprawdz tmp (dist+1);
        end;

      (* przelewanie wody z i-tej szklanki do j-tej *)
      for j = 0 to len - 1 do
        if i <> j && el.(i) <> 0 && el.(j) <> x.(j) then
          begin
            let tmp = Array.copy el in
            (* jesli nie da sie przelac calej wody *)
            if el.(i) + el.(j) > x.(j) then
              begin
                tmp.(i) <- el.(i) - (x.(j) - el.(j));
                tmp.(j) <- x.(j);
              end
            (* jesli da sie przelac w calosci wode *)
            else
              begin
                tmp.(i) <- 0;
                tmp.(j) <- el.(i) + el.(j);
              end;
            sprawdz tmp (dist+1);
          end
      done
    done
  done;
  (* zwraca wynik *)
  !wynik;;


(* przyjmuje na wejsciu tablice ar par (p_i, q_i), gdzie
   p_i oznacza pojemnosc i-tej szklanki, a q_i - stan,
   ktory ma zostac osiagniety. Funkcja oblicza, jak wiele
   krokow potrzeba, aby uzyskac wynik. Jesli nie jest to
   mozliwe, zwraca -1 *)
let przelewanka ar =
  (* zresetowanie kolejki q i mapy g *)
  Queue.clear q;
  Hashtbl.clear g;
  let len = Array.length ar in
  if len = 0 then
    0
  else
    (* podzial tablicy ar na dwie osobne - x i y *)
    let x = Array.map fst ar
    and y = Array.map snd ar in
    (* stan poczatkowy jest wynikiem *)
    if Array.for_all (fun p -> p = 0) y then
      0
    else
      (* oblicza nwd nieujmnych liczb p q *)
      let rec nwd p q =
        if p = q || q = 0 then p else
        nwd q (p mod q) in
      let gcd = Array.fold_left nwd 0 x in
      if gcd = 0 then
        -1
      else
        (* dwa warunki konieczne do istnienia rozwiazania:
           1) musi pozostac co najmniej jedna pusta lub pelna szklanka
           2) nwd pojemnosci szklanek musi dzielic ilosc wody w kazdej z nich *)
        let warunek1 = Array.exists (fun (p, q) -> p <> 0 && (q = 0 || q = p)) ar
        and warunek2 = Array.for_all (fun p -> p mod gcd = 0) y in
        
        if warunek1 && warunek2 then
          przelewaj x y
        (* jesli nie sa spelnione poprzednie warunki,
           to nie da sie osiagnac wyniku *)
        else
          -1
