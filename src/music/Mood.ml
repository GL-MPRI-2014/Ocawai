type t = int

exception NotYetInitialised

let client_data = ref None

let init client_data' =
  client_data := Some client_data'

let ll = List.length

let get_mood =
  match !client_data with
    | None -> raise NotYetInitialised
    | Some data ->
        let our_army = data#actual_player#get_army
        and enemy_army = List.flatten
                          (List.map
                            (fun x -> data#actual_player#get_visible_army_for x)
                            data#players)
        and resources = data#actual_player#get_value_resource
        and our_buildings = data#actual_player#get_buildings
        and buildings = data#neutral_buildings
        in
        (ll our_army * 10) + (ll resources) + (ll buildings) +
        (ll our_buildings * 100) - (ll enemy_army * 50)
