(* przelewanka Maciej Nowotka; review: Mikołaj Szymański, gr. 3 *)

let wylej aktualny i tbl bfs t =
  let tempi = aktualny.(i) in
  if tempi <> 0 then
    begin
      let nowy = Array.copy aktualny in
        nowy.(i) <- 0;
          if not (Hashtbl.mem tbl nowy) then 
            begin
              Queue.add nowy bfs;
              Hashtbl.add tbl nowy (t + 1);
            end; 
    end;
;;

let wlej aktualny pojemnosci i tbl bfs t =
  let tempi = aktualny.(i) in
  if tempi <> pojemnosci.(i) then
    begin
      let nowy = Array.copy aktualny in
        nowy.(i) <- pojemnosci.(i);
          if not (Hashtbl.mem tbl nowy) then 
            begin
              Queue.add nowy bfs;
              Hashtbl.add tbl nowy (t + 1);
            end; 
    end;
;;

let przelej aktualny pojemnosci i tbl bfs t =
  let tempi = aktualny.(i) in  
  if tempi <> 0 then
    begin
      let n = Array.length aktualny in
      for j = 0 to n - 1 do
        let tempj = aktualny.(j) in
        if tempj < pojemnosci.(j) && j <> i then
          begin
            let nowy = Array.copy aktualny in
            nowy.(j) <- min pojemnosci.(j) (tempi + tempj);
            nowy.(i) <- max (tempi - (pojemnosci.(j) - tempj)) 0;
            if not (Hashtbl.mem tbl nowy) then 
              begin
                Queue.add nowy bfs;
                Hashtbl.add tbl nowy (t + 1);
              end; 
            end;
      done;
    end;
;;

let solve szukany pojemnosci n = 
  let tbl = Hashtbl.create 1 in
  let bfs = Queue.create() in
  Queue.add (Array.make n 0) bfs;
  Hashtbl.add tbl (Array.make n 0) 0;
  
  while not (Queue.is_empty bfs) && not (Hashtbl.mem tbl szukany) do 
    let aktualny = Queue.take bfs in 
    begin
      let t = Hashtbl.find tbl aktualny in
      for i = 0 to n - 1 do
        wylej aktualny i tbl bfs t;
        wlej aktualny pojemnosci i tbl bfs t;
        przelej aktualny pojemnosci i tbl bfs t;
      done;
    end; 
  done; 
  
  if Hashtbl.mem tbl szukany then Hashtbl.find tbl szukany
  else -1

let rec nwd a b = if a = 0 then b else nwd (b mod a) a

let usun_0 arr = (List.filter (fun x -> x <> (0, 0)) (Array.to_list arr)) |> Array.of_list (* usuwa wartośći (0, 0) *)  

let nwd_tablicy arr = Array.fold_left (fun nwd_akt x -> nwd nwd_akt x) 0 arr 

(* 2 warunki konieczne *)
(* nwd pojemnosci dzieli oczekiwane wartosci *)
let war1 szuk nwdt = Array.fold_left (fun b x -> b && x mod nwdt = 0) true szuk 

(* jedna oczekiwana wartosc jest pelna lub pusta *)
let war2 poj szuk = 
  Array.fold_left (fun (b, i) x -> (b || (x = 0 || x = poj.(i)), i + 1)) (false, 0) szuk |> fst

let przelewanka arr =
  (* obrabianie danych wejsciowych *)
  let arr = usun_0 arr in
  let pojemnosci = Array.map fst arr in 
  let szukany = Array.map snd arr in
  let nwdt = nwd_tablicy pojemnosci in

  if Array.length arr = 0 then 0 else
    if war1 szukany nwdt && war2 pojemnosci szukany then 
      let n = Array.length arr in
      solve szukany pojemnosci n 
    else -1
