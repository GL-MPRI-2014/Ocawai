
let tiles_config = "resources/config/tiles.json"

let units_config = "resources/config/units.json"

(* Tiles config *)

let create_tile_list_from_file file =
  Ag_util.Json.from_file Tile_j.read_t_list file

let create_tile_from_file name file =
  let tiles = create_tile_list_from_file file in
  List.find
    (fun t -> Tile.get_name t = name)
    tiles

let create_tile_list () = create_tile_list_from_file tiles_config

let create_tile name = create_tile_from_file name tiles_config

(* Units config *)

let create_unbound_unit_list_from_file s1 =
  List.map Unit.create_unbound_from_parsed_unit (Ag_util.Json.from_file Unit_j.read_t_list s1)

let create_unbound_unit_from_file s1 s2 =
  List.find
    (fun uni -> uni#name = s1)
    (create_unbound_unit_list_from_file s2)

let create_unbound_unit_list () = create_unbound_unit_list_from_file units_config

let create_unbound_unit s1 = create_unbound_unit_from_file s1 units_config

(* *)


