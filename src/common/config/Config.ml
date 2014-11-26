
(* Tiles config *)

let create_tile_list_from_file file =
  List.map Tile.parsed_tile_to_tile (Ag_util.Json.from_file Tile_j.read_t_list file)

(* Units config *)

let create_unbound_unit_list_from_file file =
  List.map Unit.create_unbound_from_parsed_unit (Ag_util.Json.from_file Unit_j.read_t_list file)

(* Settings config *)

let create_settings_from_file file =
  Ag_util.Json.from_file Settings_j.read_t file

let create_engine_settings_from_file file =
  Ag_util.Json.from_file Settings_engine_j.read_t file

let create_interface_settings_from_file file =
  Ag_util.Json.from_file Settings_interface_j.read_t file

let write_settings_in_file file settings =
  Ag_util.Json.to_file Settings_j.write_t file settings

let write_engine_settings_in_file file settings =
  Ag_util.Json.to_file Settings_engine_j.write_t file settings

let write_interface_settings_in_file file settings =
  Ag_util.Json.to_file Settings_interface_j.write_t file settings

(* default parameters *)

let default_config_files = 
  ("resources/config/tiles.json",
  "resources/config/units.json",
  "resources/config/settings.json",
  "resources/config/settings_default.json")

let default_engine_settings_files = 
  ("resources/config/settings_engine.json",
  "resources/config/settings_engine_default.json")

let default_interface_settings_files = 
  ("resources/config/settings_interface.json",
  "resources/config/settings_interface_default.json")

(* config class *)
class t =
object (self)

  val mutable tiles_config = ""
  val mutable units_config = ""
  val mutable settings_temp = ""
  val mutable settings_default = ""
  val mutable interface_settings_temp = ""
  val mutable interface_settings_default = ""
  val mutable engine_settings_temp = ""
  val mutable engine_settings_default = ""
  
  val mutable t_list = (None: (Tile.t list) option)
  val mutable u_list = (None: (Unit.unbound_t list) option)
  val mutable s = (None: Settings_t.t option)
  val mutable engine_s = (None: Settings_engine_t.t option)
  val mutable interface_s = (None: Settings_interface_t.t option)
  
  method private available_settings = if Sys.file_exists settings_temp then settings_temp else settings_default
  method private available_engine_settings = if Sys.file_exists engine_settings_temp then engine_settings_temp else engine_settings_default
  method private available_interface_settings = if Sys.file_exists interface_settings_temp then interface_settings_temp else interface_settings_default
  
  method private init_names tiles_file units_file
                    settings_temp_file settings_default_file
                    engine_settings_temp_file engine_settings_default_file
                    interface_settings_temp_file interface_settings_default_file =
    if Sys.file_exists tiles_file then tiles_config <- tiles_file;
    if Sys.file_exists units_file then units_config <- units_file;
    if settings_temp_file <> "" then settings_temp <- settings_temp_file;
    if Sys.file_exists settings_default_file then settings_default <- settings_default_file;
    if engine_settings_temp_file <> "" then engine_settings_temp <- engine_settings_temp_file;
    if Sys.file_exists engine_settings_default_file then engine_settings_default <- engine_settings_default_file;
    if interface_settings_temp_file <> "" then interface_settings_temp <- interface_settings_temp_file;
    if Sys.file_exists interface_settings_default_file then interface_settings_default <- interface_settings_default_file;
    self#reload
  
  method init (tiles_file, units_file, settings_temp_file, settings_default_file) =
    self#init_names
          tiles_file units_file
          settings_temp_file settings_default_file
          "" ""
          "" ""
  
  method init_engine (engine_settings_temp_file, engine_settings_default_file) =
    self#init_names
          "" ""
          "" ""
          engine_settings_temp_file engine_settings_default_file
          "" ""
  
  method init_interface (interface_settings_temp_file, interface_settings_default_file) =
    self#init_names
            "" ""
            "" ""
            "" ""
            interface_settings_temp_file interface_settings_default_file
  
  method tiles_list = match t_list with Some a -> a | None -> failwith("no valid tiles file provided")
  method unbound_units_list = match u_list with Some a -> a | None -> failwith("no valid units file provided")
  method settings = match s with Some a -> a | None -> failwith("no valid settings file provided")
  method settings_engine = match engine_s with Some a -> a | None -> failwith("no valid engine settings file provided")
  method settings_interface = match interface_s with Some a -> a | None -> failwith("no valid interface settings file provided")
  
  method tile name = List.find (fun t -> Tile.get_name t = name) self#tiles_list
  method unbound_unit name = List.find (fun uni -> uni#name = name) self#unbound_units_list

  method reload =
    if tiles_config <> "" then t_list <- Some (create_tile_list_from_file tiles_config);
    if units_config <> "" then u_list <- Some (create_unbound_unit_list_from_file units_config);
    if self#available_settings <> "" then s <- Some (create_settings_from_file self#available_settings);
    if self#available_engine_settings <> "" then engine_s <- Some (create_engine_settings_from_file self#available_engine_settings);
    if self#available_interface_settings <> "" then interface_s <- Some (create_interface_settings_from_file self#available_interface_settings)
  
  method reset_to_default =
    if settings_default <> "" then s <- Some (create_settings_from_file settings_default);
    if engine_settings_default <> "" then s <- Some (create_settings_from_file engine_settings_default);
    if interface_settings_default <> "" then s <- Some (create_settings_from_file interface_settings_default)
  
  method save_settings =
    write_settings_in_file settings_temp self#settings;
    write_engine_settings_in_file engine_settings_temp self#settings_engine;
    write_interface_settings_in_file interface_settings_temp self#settings_interface

end

(* global config *)

let config = let conf = new t in conf#init default_config_files;conf
let init_engine () = config#init_engine default_engine_settings_files
let init_interface () = config#init_interface default_interface_settings_files
let tiles_list () = config#tiles_list
let unbound_units_list () = config#unbound_units_list
let settings () = config#settings
let settings_engine () = config#settings_engine
let settings_interface () = config#settings_interface
let tile name = config#tile name
let unbound_unit name = config#unbound_unit name
let reload () = config#reload
let reset_to_default () = config#reset_to_default
let save_settings () = config#save_settings

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

