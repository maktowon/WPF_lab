(* Autor: Maciej Nowotka; Review: Jacek Muszyński *)

(* przerabia daną listę na hashtable z parą (ilość wierzchołków wchodzących * tablica sąsiadów) *)
let convert l = 
    let a = Array.of_list (List.map (fun (vert, l_v) -> (vert, Array.of_list l_v)) l) in
    let n = List.length l - 1 in
    let tbl = Hashtbl.create n in
        for i = 0 to n do
            let vert = fst a.(i) in
            let a_v = snd a.(i) in
            if Hashtbl.mem tbl vert then
                let (edges_in, verts_out) = Hashtbl.find tbl vert in
                Hashtbl.replace tbl vert (edges_in, Array.append verts_out a_v)
            else Hashtbl.add tbl vert (0, a_v);
            let k = Array.length a_v - 1 in
            for j = 0 to k do
                if Hashtbl.mem tbl a_v.(j) then 
                    let (edges_in, verts_out) = Hashtbl.find tbl a_v.(j) in   
                    Hashtbl.replace tbl a_v.(j) (edges_in + 1, verts_out);
                else Hashtbl.add tbl a_v.(j) (1, [||]);  
            done;
        done;
    tbl
;;

(* przy przerzucaniu wierzchołka z kolejki do odpowiedzi,
 wszystkim sąsiadom zmniejszamy liczbe krawędzi wchodzących *)
let decr_edges tbl a i =
    let (edges_in, verts_out) = Hashtbl.find tbl a.(i) in
    Hashtbl.replace tbl a.(i) (edges_in - 1, verts_out);
;;

(* sprawdza czy po wykonaniu decr_edges do któregoś z sąsiadów nie 
wchodzi już żadna inna krawędź i w takim wypadku przerzuca ją do kolejki *)
let add tbl a i q =
    let (edges_in, _) = Hashtbl.find tbl a.(i) in
    if edges_in = 0 then Queue.add a.(i) q;
;;

exception Cykliczne;;

(* właściwa funkcja *)
let topol l =
    let topos = ref [] in
    let tbl = convert l in
    let kol = Queue.create () in
    Hashtbl.iter (fun vert (edges_in, _) -> if edges_in = 0 then Queue.add vert kol) tbl;
    while not (Queue.is_empty kol) do
        let vert = Queue.pop kol in
        topos := vert::!topos;
        let (_, nghbr) = Hashtbl.find tbl vert in
        let n = Array.length nghbr - 1 in
        for i = 0 to n do
          decr_edges tbl nghbr i;
          add tbl nghbr i kol;
        done;
    done;
    if Hashtbl.length tbl = List.length !topos then List.rev !topos 
    else raise Cykliczne
;;

topol [(1, []); (2, [3]); (1, [2])];;
topol [(1, [2; 3]); (3, [2]); (1, [2])];;
topol [("ala", ["ma"; "kota"]); ("ma", ["kota"])];;
topol [("ala", ["ma"; "nie"]); ("nie", ["ma"; "kota"]); ("ma", ["kota"])];;
(* cykliczne *)
(* topol [(1, [1])];; *)
(* topol [(1, [2]); (2, [3]); (3, [1])];; *) 
