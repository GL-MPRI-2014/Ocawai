type t = int

exception NotYetInitialised

let client_data = ref None
let base = ref 0
let saw_min = ref 0
let saw_max = ref 0

let ll = List.length

let p_our_army = 10
and p_enemy_army = 40
and p_resources = 1
and p_our_buildings = 20
and p_buildings = 10

let min a b = if a < b then a else b
let max a b = if a < b then b else a

let get () =
  match !client_data with
    | None -> raise NotYetInitialised
    | Some data ->
        let fog = data#actual_player#get_fog in
        let visible p = Fog.visible fog p in
        let our_army = data#actual_player#get_army
        and enemy_army = List.flatten
                          (List.map
                            (fun p ->
                              List.filter
                                (fun u -> visible u#position)
                                p#get_army)
                            data#players)
        and resources = data#actual_player#get_value_resource
        and our_buildings = data#actual_player#get_buildings
        and buildings = data#neutral_buildings
        in
        let value =
          (ll our_army * p_our_army)
        + resources * p_resources
        + (ll buildings * p_buildings)
        + (ll our_buildings * p_our_buildings)
        - ((ll enemy_army - ll our_army) * p_enemy_army)
        - (!base)
        in
        saw_min := min (!saw_min) value;
        saw_max := max (!saw_max) value;
        let intervale = float_of_int (!saw_max - !saw_min) in
        (2. *.
          ((float_of_int (value - !saw_min))
          /. (intervale)))
        -. 1.

let init (client_data' : ClientData.client_data) =
  client_data := Some client_data';
  base := int_of_float (get ())
