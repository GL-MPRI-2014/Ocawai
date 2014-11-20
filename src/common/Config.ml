
let tiles_config = "resources/config/tiles.json"

let units_config = "resources/config/units.json"

(* Tiles config *)

let create_tile_list_from_file file =
  List.map Tile.parsed_tile_to_tile (Ag_util.Json.from_file Tile_j.read_t_list file)

let create_tile_from_file name file =
  let tiles = create_tile_list_from_file file in
  List.find
    (fun t -> Tile.get_name t = name)
    tiles

let create_tile_list () = create_tile_list_from_file tiles_config

let create_tile name = create_tile_from_file name tiles_config

(* Units config *)

let create_unbound_unit_list_from_file file =
  List.map Unit.create_unbound_from_parsed_unit (Ag_util.Json.from_file Unit_j.read_t_list file)

let create_unbound_unit_from_file name file =
  List.find
    (fun uni -> uni#name = name)
    (create_unbound_unit_list_from_file file)

let create_unbound_unit_list () = create_unbound_unit_list_from_file units_config

let create_unbound_unit name = create_unbound_unit_from_file name units_config


let default_config_files = ("resources/config/tiles.json","resources/config/units.json","resources/config/settings_default.json","resources/config/settings.json")

class t (t_config,u_config,s_file,s_tmp : string*string*string*string) =
object (self)

  val mutable t_list = create_tile_list_from_file t_config
  val mutable u_list = create_unbound_unit_list_from_file u_config
  val mutable s = Ag_util.Json.from_file Settings_j.read_t (if Sys.file_exists s_tmp then s_tmp else s_file)
  
  method tiles_list = t_list
  method unbound_units_list = u_list
  method settings = s
  
  method reload_tiles =
    t_list <- create_tile_list_from_file t_config
  
  method reload_units =
    u_list <- create_unbound_unit_list_from_file u_config
  
  method reload_settings =
    s <- Ag_util.Json.from_file Settings_j.read_t (if Sys.file_exists s_tmp then s_tmp else s_file)
  
  method reset_settings =
    s <- Ag_util.Json.from_file Settings_j.read_t s_file
  
  method reload = 
    self#reload_tiles;
    self#reload_units;
    self#reload_settings
  
  method write_settings =
    Ag_util.Json.to_file Settings_j.write_t s_tmp s

end

(*
open Settings_j
let test()=
  let config = new Config.t Config.default_config_files in
  let print () = print_int config#settings.cursor_speed;print_newline() in
  
  print();
  
  config#settings.cursor_speed <- 50;
  print();
  
  config#reload;
  print();
  
  config#settings.cursor_speed <- 50;
  config#write_settings;
  print();
  
  config#reload;
  print();
  
  config#reset_settings;
  print();
  
  config#settings.cursor_speed <- 50;
  config#write_settings;
  print()
*)

