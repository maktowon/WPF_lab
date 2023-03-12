(* Autor: Maciej Nowotka; Review: Paweł Preibisch gr. II *)
type point = float * float

type kartka = point -> int

let prostokat ((x1, y1) : point) ((x2, y2) : point) : kartka = 
    fun (xp, yp) ->
        if xp <= x2 && xp >= x1 && yp <= y2 && yp >= y1 then 1 
        else 0

let sq x = x *. x

(* kwadrat odległości między dwoma punktami *)
let dist (x1, y1) (x2, y2) = 
    sq (x1 -. x2) +. sq (y1 -. y2)
        
let kolko (s : point) r : kartka =
    fun p -> if dist s p <= sq r then 1
    else 0

(* iloczyn wektorowy, określa po której stronie prostej znajduje się punkt *)
let il_wek (x1, y1) (x2, y2) (xp, yp) =
    (x1 -. xp) *. (y2 -. yp) -. (x2 -. xp) *. (y1 -. yp)

(* funkcje pomocnicze do symetrii punktu względem prostej *)
(* zwraca współczynniki prostej przechodzącej prez dwa punkty *)
let prosta (x1, y1) (x2, y2) =
    let a = (y1 -. y2) /. (x1 -. x2) in
    let b = (y1 -. a *. x1) in
    (a, b)

(* zwraca współczynniki prostej prostopadłej przechodzącej przez punkt *)
let prostopadla m (x, y)=
    let a = (-1.) /. m in
    let b = (y -. a *. x) in
    (a, b)

(* zwraca punkt przecięcia się dwóch prostych *)
let przeciecie (a1, b1) (a2, b2) = 
    let x = (b2 -. b1) /. (a1 -. a2) in
    let y = a1 *. x +. b1 in
    (x, y)   

(* zwraca punkt symetryczny wzgledem prostej *)
let p_sym (x1, y1) (x2, y2) (xp, yp) = 
    if x1 = x2 then (2. *. x1 -. xp, yp)
    else if y1 = y2 then (xp, 2. *. y1 -. yp)
    else 
        let (a1, b1) = prosta (x1, y1) (x2, y2) in
        let (a2, b2) = prostopadla a1 (xp, yp) in
        let (x, y) = przeciecie (a1, b1) (a2, b2) in
        (2. *. x -. xp, 2. *. y -. yp)

let zloz (a : point) (b : point) (k : kartka) : kartka =
    fun p -> 
    let w = il_wek a b p in
    if w < 0. then 0 
    else if w > 0. then k p + k (p_sym a b p) 
    else k p

let skladaj l k = List.fold_left (fun acc (a, b) -> zloz a b acc) k l

(* testy *)

let k = prostokat (0., 0.) (4., 4.);;
let z = [((0., 2.), (1., 2.)); ((0., 3.), (1., 3.))];;
let w = skladaj z k;;
let a = w (3.5, 3.5);;
assert (a = 4);;
let a = w (3., 3.);;
assert (a = 2);;
let a = w (0., 0.);;
assert (a = 0);;

let k = kolko (0., 0.) 1.;;
let z = [((0., 0.), (0., 1.)); ((-0.5, 0.), (-0.5, 1.))];;
let w = skladaj z k;;
let a = w (-0.8, 0.8);;
assert (a = 2);;
let a = w (-0.5, 0.);;
assert (a = 2);;
let a = w (-0.7, 0.7);;
assert (a = 4);;
