type environment = (string * ScriptValues.value) list

let expose f s t env = 
  Checker.expose t s;
  (s,f)::env

let rec find s = function
  |[] -> assert false
  |(s',t)::q -> if s = s' then t else find s q

let get_logic_player = function
  |`Player(p) -> p
  | _ -> assert false

let get_list = function
  |`List(l) -> l
  | _ -> assert false

let position_to_pair p =
  let (a,b) = Position.topair p in
  `Pair(`Int a, `Int b)

let get_map = function
  |`Map(m) -> m
  | _ -> assert false

let get_functions env = 
  let self = get_logic_player (find "self" env) in
  let map  = get_map (find "map" env) in
  let plrs = List.map get_logic_player (get_list (find "players" env)) in 

  (* Functions *)
  let scr_dijkstra = 
    `Fun(function
      |`Soldier(u) -> `Fun(function
        |`Pair(`Int(a), `Int(b)) ->
            Path.dijkstra map u#position u#movement_type (Position.create (a,b))
            |> (function
                |Some(i,p) -> Path.get_move p
                | _ -> assert false)
            |> List.map position_to_pair
            |> fun l -> `List l
        | _ -> assert false
        )
      | _ -> assert false
      )
  in

  let scr_accessibles =
    `Fun(function
      |`Soldier(u) -> 
        fst (Logics.available_positions u self plrs map)
        |> List.map position_to_pair
        |> fun l -> `List l
      | _ -> assert false
      )
  in

  let scr_inrange =
  `Fun(function
    |`Pair(`Int(a), `Int(b)) -> `Fun(function
      |`Pair(`Int(minr),`Int(maxr)) ->
        Logics.units_inrange (Position.create (a,b)) (minr,maxr) self plrs
        |> List.map (function u -> `Soldier(u))
        |> (fun l -> `List l)
      | _ -> assert false
      )
    | _ -> assert false
    )
  in

  let scr_armyof =
    `Fun(function
      |`Player(p) ->
          p#get_visible_army_for self
          |> List.map (fun u -> `Soldier u)
          |> fun l -> `List l
      | _ -> assert false
    )
  in

  let scr_unit_count = 
    `Fun(function
      |`Player(p) -> `Fun(function
        |`String(s) -> 
            p#get_visible_army_for self
            |> List.fold_left 
              (fun v u -> v + (if u#name = s then 1 else 0)) 0
            |> fun l -> `Int l
        | _ -> assert false
        )
      |_ -> assert false
    )
  in

  let scr_building_count : ScriptValues.value = 
    `Fun(function
      |`Player(p) -> `Fun(function
        |`String(s) ->
            p#get_buildings
            |> List.fold_left 
              (fun v u -> v + (if u#name = s then 1 else 0)) 0
            |> fun l -> `Int l 
        | _ -> assert false
        )
      |_ -> assert false
    )
  in

  let scr_buildingsof : ScriptValues.value = 
    `Fun(function
      |`Player(p) ->
          p#get_buildings
          |> List.map (fun u -> `Building u)
          |> fun l -> `List l
      | _ -> assert false
    )
  in

  let intpair = `Pair_t(`Int_t, `Int_t) in
  (* New environment *)
  env 
  |> expose scr_dijkstra "dijkstra_to" 
    (`Fun_t (`Soldier_t, `Fun_t (intpair, `List_t(intpair))))
  |> expose scr_accessibles "accessible_positions"
    (`Fun_t (`Soldier_t, `List_t(intpair)))
  |> expose scr_inrange "ennemies_in_range"
    (`Fun_t (intpair, `Fun_t(intpair, `List_t(`Soldier_t))))
  |> expose scr_armyof "army_of"
    (`Fun_t(`Player_t, `List_t(`Soldier_t)))
  |> expose scr_buildingsof "buildings_of"
    (`Fun_t(`Player_t, `List_t(`Building_t)))
  |> expose scr_building_count "building_count"
    (`Fun_t(`Player_t, `Fun_t(`String_t, `Int_t)))
  |> expose scr_unit_count "unit_count"
    (`Fun_t(`Player_t, `Fun_t(`String_t, `Int_t)))
