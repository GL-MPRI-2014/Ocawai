exception Config_error of string

let check_error file f p t = 
begin
  match f p t with
  | None -> t 
  | Some e -> raise (Config_error (file^" : "^Ag_util.Validation.string_of_error e))
end

(* Tiles config *)

let create_valid_parsed_tile_list file =
  let t = Ag_util.Json.from_file Tile_j.read_t_list file in 
  check_error file Tile_v.validate_t_list [] t

let create_tile_list_from_file file =
  List.map Tile.parsed_tile_to_tile (create_valid_parsed_tile_list file)

(* Units config *)

let create_valid_parsed_unit_list file =
  let t = Ag_util.Json.from_file Unit_j.read_t_list file in 
  check_error file Unit_v.validate_t_list [] t

let create_unbound_unit_list_from_file file =
  List.map Unit.create_unbound_from_parsed_unit (create_valid_parsed_unit_list file)

(* Settings config *)

let create_settings_from_file file =
  let t = Ag_util.Json.from_file Settings_j.read_t file in
  check_error file Settings_v.validate_t [] t

let create_engine_settings_from_file file =
  let t = Ag_util.Json.from_file Settings_engine_j.read_t file in
  check_error file Settings_engine_v.validate_t [] t

let create_interface_settings_from_file file =
  let t = Ag_util.Json.from_file Settings_interface_j.read_t file in
  check_error file Settings_interface_v.validate_t [] t

let write_settings_in_file file settings =
  Ag_util.Json.to_file Settings_j.write_t file settings

let write_engine_settings_in_file file settings =
  Ag_util.Json.to_file Settings_engine_j.write_t file settings

let write_interface_settings_in_file file settings =
  Ag_util.Json.to_file Settings_interface_j.write_t file settings

(* default parameters *)

let base = ref ""

(*let baise_folder_giver () =*)let _= (
  let folder = ref "" in
  (try
    if (Sys.is_directory "resources/config") then
      folder := "resources/config/"
    else folder := "/usr/share/GL_2014/config/"
  with Sys_error _ -> folder := "/usr/share/GL_2014/config/";);
  base := !folder)

let default_config_files = 
  (!base ^ "tiles.json",
   !base ^ "units.json",
   !base ^ "settings.json",
   !base ^ "settings_default.json")

let default_engine_settings_files = 
  (!base ^ "settings_engine.json",
   !base ^ "settings_engine_default.json")

let default_interface_settings_files = 
  (!base ^ "settings_interface.json",
   !base ^ "settings_interface_default.json")

let default_files = 
  let (x1,x2,x3,x4) = default_config_files in
  let (x5,x6) = default_engine_settings_files in
  let (x7,x8) = default_interface_settings_files in
  (x1,x2,x3,x4,x5,x6,x7,x8)

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
  
  method private init_all (x1,x2,x3,x4,x5,x6,x7,x8) = self#init_names x1 x2 x3 x4 x5 x6 x7 x8
  
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
  
  method init_default =
    self#init_all default_files
  
  method tiles_list = match t_list with Some a -> a | None -> failwith("no valid tiles file provided so far, call init before")
  method unbound_units_list = match u_list with Some a -> a | None -> failwith("no valid units file provided so far, call init before")
  method settings = match s with Some a -> a | None -> failwith("no valid settings file provided so far, call init before")
  method settings_engine = match engine_s with Some a -> a | None -> failwith("no valid engine settings file provided so far, call init_engine before")
  method settings_interface = match interface_s with Some a -> a | None -> failwith("no valid interface settings file provided so far, call init_interface before")
  
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
    if engine_settings_default <> "" then engine_s <- Some (create_engine_settings_from_file engine_settings_default);
    if interface_settings_default <> "" then interface_s <- Some (create_interface_settings_from_file interface_settings_default)
  
  method save_settings =
    if s <> None then write_settings_in_file settings_temp self#settings;
    if engine_s <> None then write_engine_settings_in_file engine_settings_temp self#settings_engine;
    if interface_s <> None then write_interface_settings_in_file interface_settings_temp self#settings_interface

end

let config = new t

let _ = config#init_default
(* Test *)

(*
open Settings_j
let test game = 
  let print () = print_int game#config#settings.cursor_speed;print_newline() in
  
  print();
  
  game#config#settings.cursor_speed <- 50;
  print();
  
  game#config#reload;
  print();
  
  game#config#settings.cursor_speed <- 50;
  game#config#save_settings;
  print();
  
  game#config#reload;
  print();
  
  game#config#reset_to_default;
  print();
  
  game#config#settings.cursor_speed <- 50;
  game#config#save_settings;
  print()
*)

