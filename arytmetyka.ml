(* autor: Maciej Nowotka *)
(* code reviewer: Wojtek Noskowski *)

(* typ*)
type wartosc =  Przedzial of (float * float) | Dopelnienie of (float * float) | Pusty

(* mnozenie na potrzeby zadania, chcemy zeby inf * 0 = 0 *)
let ( *. ) a b =
    if a = 0. || b = 0. then 0.
    else a*.b;;

(* konstruktory *)
let wartosc_dokladnosc x p = 
    if x > 0. then Przedzial(x -. x *. (p /. 100.), x +. x *. (p /. 100.))
    else Przedzial(x +. x *. (p /. 100.), x -. x *. (p /. 100.));;

let wartosc_od_do x y = Przedzial (x, y);;

let wartosc_dokladna x = Przedzial(x, x);;

(* selektory *)
let in_wartosc x y =
    match x with
    |Przedzial(a, b) -> a <= y && y <= b
    |Dopelnienie(a, b) -> y <= a || b <= y
    |Pusty -> false;;

let min_wartosc x = 
    match x with
    |Przedzial(a, _) -> a
    |Pusty -> nan
    |_ -> neg_infinity;;

let max_wartosc x = 
    match x with
    |Przedzial(_, b) -> b
    |Pusty -> nan
    |_ -> infinity;;

let sr_wartosc x =
    match x with
    |Przedzial(a, b) -> (a +. b) /. 2.
    |_ -> nan;;

(* funkcja pomocnicza do porownywania koncow przedzialu *)
let porownaj l p = 
    if l < p then Dopelnienie(l, p)
    else Przedzial(neg_infinity, infinity);;
(* przez l p oznaczam lewy i prawy koniec przedzialu *)

(* modyfikatory *)
let plus x y = 
    match x, y with
    |Pusty, _ -> Pusty
    |_, Pusty -> Pusty
    |Przedzial(a, b), Przedzial(c, d) -> Przedzial(a +. c, b +. d)
    |Dopelnienie(a, b), Przedzial(c, d) | Przedzial(c, d), Dopelnienie(a, b) -> 
        let l = a +. d in 
        let p = b +. c in 
        porownaj l p
    |Dopelnienie(a, b), Dopelnienie(c, d) -> Przedzial(neg_infinity, infinity);;

let minus x y =
    match x, y with
    |Pusty, _ -> Pusty
    |_, Pusty -> Pusty
    |Przedzial(a, b), Przedzial(c, d) -> Przedzial(a -. d, b -. c)
    |Przedzial(a, b), Dopelnienie(c, d) -> 
        let l = b -. d in 
        let p = a -. c in 
        porownaj l p
    |Dopelnienie(a, b), Przedzial(c, d) -> 
        let l = a -. c in
        let p = b -. d in
        porownaj l p
    |Dopelnienie(a, b), Dopelnienie(c, d) -> Dopelnienie(neg_infinity, infinity);; 

let razy x y = 
    match x, y with
    |Pusty, _ -> Pusty
    |_, Pusty -> Pusty
    |Przedzial(a, b), Przedzial(c, d) -> 
        let l = min (min(a *. c) (a *. d)) (min(b *. c) (b *. d)) in
        let p = max (max(a *. c) (a *. d)) (max(b *. c) (b *. d)) in
        if l < p then Przedzial(l, p)
        else Przedzial(p, l)
    |Dopelnienie(a, b), Przedzial(c, d) | Przedzial(c, d), Dopelnienie(a, b) ->
        (* przedzial jest dodatni *)
        if(c >= 0. && d > 0.) then 
            let l = max (a *. c) (a *. d) in
            let p = min (b *. c) (b *. d) in
            porownaj l p
        (* przedzial jest ujemny *)
        else if(c < 0. && d <= 0.) then
            let l = max (b *. c) (b *. d) in
            let p = min (a *. c) (a *. d) in
            porownaj l p
        (* przedzial przechodzi przez 0 *)
        else if(c *. d < 0.) then Przedzial(neg_infinity, infinity)
        (* został przypadek gdzie przedial to (0; 0) *)
        else Przedzial(0., 0.)
    |Dopelnienie(a, b), Dopelnienie(c, d) ->
        (* jedno z dopelnien ma te same znaki *)
        if(a *. b > 0. || c *. d > 0.) then Przedzial(neg_infinity, infinity)
        else
            let l = max (a *. d) (b *. c) in
            let p = min (a *. c) (b *. d) in
            porownaj l p;;

let podzielic x y =
    match x with
    |Pusty -> Pusty
    |x ->
        match y with
        |Pusty -> Pusty
        |Przedzial(c, d) ->
            if c = 0. && d = 0. then Pusty
            (* jesli c = 0 to d > 0 *)
            else if c = 0. then razy x (Przedzial(1. /. d, infinity))   
            (* jesli d = 0 to c < 0 *)
            else if d = 0. then razy x (Przedzial(neg_infinity, 1. /. c))
            (* zbior R przechodzi na zbior R *)
            else if c = neg_infinity && d = infinity then razy x (Przedzial(neg_infinity, infinity))
            else if c = neg_infinity then
                (* sa tego samego znaku *)
                if d < 0. then razy x (Przedzial(1. /. d, 0.))
                (* sa roznych znakow *)
                else razy x (Dopelnienie(0., 1. /. d))
            else if d = infinity then
                (* sa tego samego znaku *)
                if  c > 0. then razy x (Przedzial(0., 1. /. c))
                (* sa roznych znakow *)
                else razy x (Dopelnienie(1. /. c, 0.))
            (* zeby ten warunek zachodzil musza byc tego samego znaku
               czyli przedzial nie zawiera 0 *)
            else if 1. /. d <= 1. /. c then razy x (Przedzial(1. /. d, 1. /. c))
            (* 0 nalezy do przedzialu *)
            else razy x (Dopelnienie(1. /. c, 1. /. d))
        |Dopelnienie(c, d) ->
            if c *. d > 0. then razy x (Dopelnienie(1. /. d, 1. /. c))
            else if c = d then razy x (Przedzial(neg_infinity, infinity))
            else if c = 0. then razy x (Przedzial(neg_infinity, 1. /. d))
            else if d = 0. then razy x (Przedzial(1. /. c, infinity))
            else razy x (Przedzial(1. /. d, 1. /. c));;
(* nie zapisywalem wlasnych testow,
   nastepnym razem wstawie *)
