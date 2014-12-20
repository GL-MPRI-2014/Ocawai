type t =
  | Flatman
  | Blub
  | Limboy

let to_string = function
  | Flatman -> "flatman"
  | Blub    -> "blub"
  | Limboy  -> "limboy"

let default_character = Flatman

let handler = object(self)

  val table = Hashtbl.create 13

  method character_from_id id =
    try Hashtbl.find table id
    with Not_found -> default_character

  method character_of (p:Player.logicPlayer) =
    self#character_from_id p#get_id

  method texture_from_id id name =
    (to_string (self#character_from_id id))
    ^ "_"
    ^ name

  method init constraints player_ids =
    (* Resets mapping *)
    Hashtbl.reset table;
    (* List of characters available *)
    (* By default, we always use new characters when choosing randomly *)
    let available = ref [ Flatman ; Blub ; Limboy ] in
    (* Satisfying the constraints *)
    List.iter (fun (id,chara) ->
        Hashtbl.add table id chara ;
        available := List.filter (fun c -> c <> chara) (!available)
      ) constraints ;
    (* Now taking care of all other players *)
    List.iter (fun id ->
        let c = List.nth (!available) (Random.int (List.length !available)) in
        Hashtbl.add table id c ;
        available := List.filter (fun chara -> c <> chara) (!available)
      ) (List.filter (fun id -> not (Hashtbl.mem table id)) player_ids)

  initializer
    Random.self_init ()

end
