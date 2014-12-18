(* Several functions relative to fog *)
(* To be implemented somewhere *)

type t = int array array
(* The fog is a double array of int of the size of the map,
if the value of a case is 0, it is hidden. When a unit can see a case, we increment 
the value of the case. When a unit cannot see anymore a case (moved/killed), we decrement the value of the case *)


(* With k=1, this functions modify the fog like if a new unit appears on p with range of vision
        k = -1, this functions modify the fog like if a unit with range was killed at position p*)
let set_fog_for_unit (fog: t) (p:Position.t) range k=
    let (x,y) = Position.topair p in
    let (size_x,size_y)= (Array.length fog,Array.length fog.(0)) in
    fog.(x).(y) <- fog.(x).(y) +1;

    for i=1 to range do
        if (x+i) < size_x then fog.(x+i).(y) <- fog.(x+i).(y) +k;
        if (x-i) >= 0 then fog.(x-i).(y) <- fog.(x-i).(y) +k
    done;
    for j=1 to range do
        if (y+j) < size_y then fog.(x).(y+j) <- fog.(x).(y+j) +k;
        if (y-j) >=0  then fog.(x).(y-j) <- fog.(x).(y-j) +k
    done;
    for i=1 to range do
        for j=1 to range do
            if (i+j) <= range && (i+j)>0 then
            (
                if (x+i) < size_x then
                (
                if (y+j) < size_y then fog.(x+i).(y+j) <- fog.(x+i).(y+j)+1;
                if (y-j) >= 0 then fog.(x+i).(y-j) <- fog.(x+i).(y-j)+1;
                );
                if (x-i) >= 0 then
                (
                if (y+j) < size_y then fog.(x-i).(y+j) <- fog.(x-i).(y+j)+1;
                if (y-j) >=0 then fog.(x-i).(y-j) <- fog.(x-i).(y-j) +1;
                )
            )
        done;
    done

let add_unit_fog fog (p:Position.t) range= set_fog_for_unit fog p range 1
let delete_unit_fog fog (p:Position.t) range= set_fog_for_unit fog p range (-1)


(*
let update_fog fog (u:Types.update) = 
    match u with
        | Set_army(u_list,id) -> List.iter (fun x -> add_unit_fog fog x#position x#vision_range) u_list;
        | Add_unit(u,id)->add_unit_fog fog u#position u#vision_range
       (* | Delete_unit(u,id)->delete_unit_fog u#position u#vision_range
        | Move_unit(u,mov,id) -> (delete_unit_fog (List.head mov) u#vision_range;
                                  add_unit_fog fog (List.head (List.rev mov)) u#vision_range) *)

let unit_is_visible fog (u:Unit.t)=
    let (x,y) = Position.topair u#position in
        fog.(x).(y) = 0
let apply_fog_to_update fog (u:Types.update) = 
    match u with
        | Set_army(u_list,id) -> (let res = ref [] in List.iter (fun x -> if unit_is_visible then res := x::!res) u_list;
                                  Set_army(!res,id)
        | Add_unit(u,id)->if unit_is_
       (* | Delete_unit(u,id)->delete_unit_fog u#position u#vision_range
        | Move_unit(u,mov,id) -> (delete_unit_fog (List.head mov) u#vision_range;
                                  add_unit_fog fog (List.head (List.rev mov)) u#vision_range) *) 
*)


(*Passer par les updates a l'air plutot compliqué, voir impossible. Types d'updates non cohérent, parfois unit, parfois unit_id. De plus, si par exemple une unit enemie arrive soudain dans notre champ de vision. Ben, si tout ce qu'on reçoit, c'est l'udpate contenant un mouvement troncqué, on aura l'id d'une unité qu'on ne connait pas.    
Proposition, implémenté un mask niveau interface graphique pour les players normaux, et pour les IA, modifié quelques fonctions de ScriptCore (ex scr_armyof) pour qu'elles renvoie les informations partiels. *)
(* Le fog deviant une value de la Class Player ? *)

(*
let init_fog (player:Player.logicPlayer) (field: Battlefield.t) = 
    let (size_x,size_y) = Battlefield.size field in
    let fog = Array.make_matrix size_x size_y 0 in
    List.iter (fun x -> add_unit_fog fog x#position x#vision_range) player#get_army;
    fog
*)
