(* Autor: Maciej Nowotka; Review: Paweł Gadziński *)

(*Kolejka of (wartosc (w) * glebokosc prawego (d) * lewe poddrzewo (l) * prawe poddrzewo (r) *)
type 'a queue = 
    |Kolejka of ('a * int * 'a queue * 'a queue)
    |Pusta;;

let empty = Pusta;;
let is_empty q = q = Pusta;;
exception Empty;;

let rec join q p = 
    match q, p with 
        |Kolejka(wq, dq, lq, rq), Kolejka(wp, _, _, _) ->
            if wq <= wp then
                let t = join rq p in
                match lq, t with
                |Kolejka(_, dlq, _, _), Kolejka(_, dt, _, _) -> 
                    if dlq < dt then Kolejka(wq, dlq + 1, t, lq) 
                    else Kolejka(wq, dt + 1, lq, t)
                |Pusta, t -> Kolejka (wq, 0, t, Pusta)
                |lq, Pusta -> Kolejka (wq, 0, lq, Pusta)
            else join p q 
        |Pusta, Pusta -> Pusta
        |Pusta, _ -> p
        |_, Pusta -> q;;

let add e q =
    join (Kolejka(e, 0, Pusta, Pusta)) q;;

let delete_min q =
    match q with
    |Kolejka(wq, _, lq, rq) -> (wq, join lq rq)
    |Pusta -> raise Empty;;

(* testy *)

(*let t1 = empty;;
let t2 = empty;;

let t1 = add 5 t1;;
let t1 = add 10 t1;;
let t1 = add 12 t1;;

let t2 = add 14 t2;;
let t2 = add 7 t2;;
let t2 = add 3 t2;;
let t2 = add 8 t2;;

let (w, t4) = delete_min t1;;
assert (w = 5);;

let (w, t4) = delete_min t4;;
assert (w = 10);;

let (w, t4) = delete_min t4;;
assert (w = 12);;

let (w, t4) = delete_min t2;;
assert (w = 3);;

let (w, t4) = delete_min t4;;
assert (w = 7);;

let (w, t4) = delete_min t4;;
assert (w = 8);;

let (w, t4) = delete_min t4;;
assert (w = 14);;

let t3 = join t1 t2;;

let (w, t4) = delete_min t3;;
assert (w = 3);;

let (w, t4) = delete_min t4;;
assert (w = 5);;

let (w, t4) = delete_min t4;;
assert (w = 7);;

let (w, t4) = delete_min t4;;
assert (w = 8);;

let (w, t4) = delete_min t4;;
assert (w = 10);;

let (w, t4) = delete_min t4;;
assert (w = 12);;

let (w, t4) = delete_min t4;;
assert (w = 14);;

let b = is_empty t4;;
assert (b = true);;

(* --- --- --- --- --- *)

let t1 = empty;;
let t2 = empty;;

let t1 = add (-7) t1;;
let t1 = add (-20) t1;;
let t1 = add (-30) t1;;

let t2 = add 7 t2;;
let t2 = add 20 t2;;
let t2 = add 40 t2;;
let t2 = add 8 t2;;

let (w, t4) = delete_min t1;;
assert (w = -30);;

let (w, t4) = delete_min t4;;
assert (w = -20);;

let (w, t4) = delete_min t4;;
assert (w = -7);;

let (w, t4) = delete_min t2;;
assert (w = 7);;

let (w, t4) = delete_min t4;;
assert (w = 8);;

let (w, t4) = delete_min t4;;
assert (w = 20);;

let (w, t4) = delete_min t4;;
assert (w = 40);;

let (w, t4) = delete_min t1;;
assert (w = -30);;

let t3 = join t1 t2;;

let (w, t4) = delete_min t3;;
assert (w = -30);;

let (w, t4) = delete_min t4;;
assert (w = -20);;

let (w, t4) = delete_min t4;;
assert (w = -7);;

let (w, t4) = delete_min t4;;
assert (w = 7);;

let (w, t4) = delete_min t4;;
assert (w = 8);;

let (w, t4) = delete_min t4;;
assert (w = 20);;

let (w, t4) = delete_min t4;;
assert (w = 40);;

let b = is_empty t4;;
assert (b = true);; *) 
