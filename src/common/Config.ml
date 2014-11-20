
(* Tiles config *)

let create_tile_list_from_file file =
  List.map Tile.parsed_tile_to_tile (Ag_util.Json.from_file Tile_j.read_t_list file)

let create_tile_from_file name file =
  let tiles = create_tile_list_from_file file in
  List.find
    (fun t -> Tile.get_name t = name)
    tiles

(* Units config *)

let create_unbound_unit_list_from_file file =
  List.map Unit.create_unbound_from_parsed_unit (Ag_util.Json.from_file Unit_j.read_t_list file)

let create_unbound_unit_from_file name file =
  List.find
    (fun uni -> uni#name = name)
    (create_unbound_unit_list_from_file file)

(* Settings config *)

let create_settings_from_file file = Ag_util.Json.from_file Settings_j.read_t file

(* default parameters *)

let default_config_files = 
  ("resources/config/tiles.json",
  "resources/config/units.json",
  "resources/config/settings.json",
  "resources/config/settings_default.json")

(* config class *)

class t (t_config,u_config,s_tmp,s_file : string*string*string*string) =
object (self)

  val mutable t_list = create_tile_list_from_file t_config
  val mutable u_list = create_unbound_unit_list_from_file u_config
  val mutable s = create_settings_from_file (if Sys.file_exists s_tmp then s_tmp else s_file)
  
  method tiles_list = t_list
  method tile name = List.find (fun t -> Tile.get_name t = name) t_list
  
  method unbound_units_list = u_list
  method unbound_unit name = List.find (fun uni -> uni#name = name) u_list

  method settings = s
  
  method reload_tiles =
    t_list <- create_tile_list_from_file t_config
  
  method reload_units =
    u_list <- create_unbound_unit_list_from_file u_config
  
  method reload_settings =
    s <- create_settings_from_file (if Sys.file_exists s_tmp then s_tmp else s_file)
  
  method reset_settings =
    s <- create_settings_from_file s_file
  
  method reload = 
    self#reload_tiles;
    self#reload_units;
    self#reload_settings
  
  method write_settings =
    Ag_util.Json.to_file Settings_j.write_t s_tmp s

end

(* Test *)

(*
open Settings_j
let test() = 
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

