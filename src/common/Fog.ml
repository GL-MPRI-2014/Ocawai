(* Several functions relative to fog *)

(* The fog is a double array of int of the size of the map,
if the value of a case is 0, it is hidden. When a unit can see a case,
we increment the value of the case. When a unit cannot see anymore a case
(moved/killed), we decrement the value of the case *)
type t = int array array


let init w h =
  Array.make_matrix w h 0

let copy fog =
  let copy_matrix a =
    if a = [||] then [||]
    else begin
      let n = Array.length a
      and m = Array.length (a.(0)) in
      let a' = Array.make_matrix n m (a.(0).(0)) in
      for i = 0 to n - 1 do
        for j = 0 to m - 1 do
          a'.(i).(j) <- a.(i).(j)
        done
      done ;
    a'
    end
  in
  copy_matrix fog

(* Allows to secure a function with respect to the fog initialization *)
let secure f a fail =
  try f a
  with Invalid_argument "index out of bounds" -> fail ()

let silent () = ()

let fail_bool b () = b

let fail_false () = fail_bool false ()


(* With k=1, this functions modify the fog like if a new unit appears on p
   with range of vision
        k = -1, this functions modify the fog like if a unit with range was
   killed at position p*)
let set_fog_for_unit (fog: t) (p:Position.t) range k =
    let (x,y) = Position.topair p in
    let (size_x,size_y)= (Array.length fog,Array.length fog.(0)) in
    (* On commence par s'occuper de la croix autour du joueur *)
    fog.(x).(y) <- fog.(x).(y) + k;
    for i=1 to range do
        if (x+i) < size_x then fog.(x+i).(y) <- fog.(x+i).(y) +k;
        if (x-i) >= 0 then fog.(x-i).(y) <- fog.(x-i).(y) +k
    done;
    for j=1 to range do
        if (y+j) < size_y then fog.(x).(y+j) <- fog.(x).(y+j) +k;
        if (y-j) >=0  then fog.(x).(y-j) <- fog.(x).(y-j) +k
    done;
    (* Puis on construit le reste avec les symétries *)
    for i=1 to range do
        for j=1 to range do
            if (i+j) <= range && (i+j)>0 then
            (
                if (x+i) < size_x then
                (
                if (y+j) < size_y then fog.(x+i).(y+j) <- fog.(x+i).(y+j)+k;
                if (y-j) >= 0 then fog.(x+i).(y-j) <- fog.(x+i).(y-j)+k;
                );
                if (x-i) >= 0 then
                (
                if (y+j) < size_y then fog.(x-i).(y+j) <- fog.(x-i).(y+j)+k;
                if (y-j) >=0 then fog.(x-i).(y-j) <- fog.(x-i).(y-j) +k;
                )
            )
        done
    done

(* update du fog quand on ajoute une unité *)
let add_entity fog (p:Position.t) range =
  secure (set_fog_for_unit fog p range) 1 silent

(* update du fog quand on delete une unité *)
let remove_entity fog (p:Position.t) range =
  secure (set_fog_for_unit fog p range) (-1) silent

let hidden fog pos =
  let (i,j) = Position.topair pos in
  secure (fun () -> fog.(i).(j) = 0) () fail_false

let visible fog pos =
  not (hidden fog pos)

let hidden_unit fog (u:Unit.t) =
  hidden fog u#position

let hidden_building fog (b:Building.t) =
  hidden fog b#position

let visible_unit fog u =
  not (hidden_unit fog u)

let visible_building fog b =
  not (hidden_building fog b)

let visible_army fog =
  List.filter (visible_unit fog)

let visible_buildings fog =
  List.filter (visible_building fog)
