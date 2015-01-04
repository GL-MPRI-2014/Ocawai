type t = int

exception NotYetInitialised

let client_data = ref None

let init (client_data' : ClientData.client_data) =
  client_data := Some client_data'

let ll = List.length

let p_our_army = 10
and p_enemy_army = 100
and p_resources = 1
and p_our_buildings = 20
and p_buildings = 10

let get () =
  match !client_data with
    | None -> raise NotYetInitialised
    | Some data ->
        let fog = data#actual_player#get_fog in
        let visible p =
          let (i,j) = Position.topair p in
          if Array.length fog > 0 then fog.(i).(j) <> 0 else true
        in
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
          (ll our_army * p_our_army)
        + resources * p_resources
        + (ll buildings * p_buildings)
        + (ll our_buildings * p_our_buildings)
        - ((ll enemy_army - ll our_army) * p_enemy_army)
