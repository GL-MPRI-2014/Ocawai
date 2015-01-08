open Settings_t
open Settings_engine_t
open Settings_interface_t

module ConfigLog = Log.Make (struct let section = "Config" end)

exception Missing_config of string

(* Config validity checking*)
exception Config_error of string

let check_error file f p t =
begin
  match f p t with
  | None -> t
  | Some err -> raise (Config_error (file^" : "^Ag_util.Validation.string_of_error err))
end

(* Tiles config *)

let create_valid_parsed_tiles_list_from_file file config_name =
  let open Tile_t in
  let t = Ag_util.Json.from_file Tile_j.read_t_list file in
  let tr = check_error file Tile_v.validate_t_valid_list [] {list = t;} in
  ConfigLog.infof "%s[loaded] tiles_list : %s" config_name file;
  tr.list

let create_tiles_list_from_file file config_name =
  List.map Tile.parsed_tile_to_tile (create_valid_parsed_tiles_list_from_file file config_name)

let create_valid_parsed_tiles_list_from_string s config_name =
  let open Tile_t in
  let t = Tile_j.t_list_of_string s in
  let tr = check_error "tile" Tile_v.validate_t_valid_list [] {list = t;} in
  ConfigLog.infof "%s[loaded] tiles_list" config_name;
  tr.list

let create_tiles_list_from_string s config_name =
  List.map Tile.parsed_tile_to_tile (create_valid_parsed_tiles_list_from_file s config_name)

let string_of_tiles_list tiles_list =
  Tile_j.string_of_t_list (List.map Tile.tile_to_parsed_tile tiles_list)

(* Units config *)

let create_valid_parsed_units_list file config_name =
  let open Unit_t in
  let t = Ag_util.Json.from_file Unit_j.read_t_list file in
  let tr = check_error file Unit_v.validate_t_valid_list [] {list = t;} in
  ConfigLog.infof "%s[loaded] unbound_units_list : %s" config_name file;
  tr.list

let create_unbound_units_list_from_file file config_name =
  List.map Unit.create_unbound_from_parsed_unit (create_valid_parsed_units_list file config_name)

let create_valid_parsed_units_list_from_string s config_name =
  let open Unit_t in
  let t = Unit_j.t_list_of_string s in
  let tr = check_error "units" Unit_v.validate_t_valid_list [] {list = t;} in
  ConfigLog.infof "%s[loaded] unbound_units_list" config_name;
  tr.list

let create_unbound_units_list_from_string s config_name =
  List.map Unit.create_unbound_from_parsed_unit (create_valid_parsed_units_list_from_string s config_name)

let string_of_unbound_units_list units_list =
  Unit_j.string_of_t_list (List.map Unit.create_parsed_unit_from_unbound units_list)

(* Buildings config *)

let create_valid_parsed_buildings_list file config_name =
  let open Building_t in
  let t = Ag_util.Json.from_file Building_j.read_t_list file in
  let tr = check_error file Building_v.validate_t_valid_list [] {list = t;} in
  ConfigLog.infof "%s[loaded] unbound_buildings_list : %s" config_name file;
  tr.list

let create_unbound_buildings_list_from_file file config_name =
  List.map Building.create_unbound_from_parsed_building (create_valid_parsed_buildings_list file config_name)

let create_valid_parsed_buildings_list_from_string s config_name =
  let open Building_t in
  let t = Building_j.t_list_of_string s in
  let tr = check_error "buildings" Building_v.validate_t_valid_list [] {list = t;} in
  ConfigLog.infof "%s[loaded] unbound_buildings_list" config_name;
  tr.list

let create_unbound_buildings_list_from_string s config_name =
  List.map Building.create_unbound_from_parsed_building (create_valid_parsed_buildings_list_from_string s config_name)

let string_of_unbound_buildings_list buildings_list =
  Building_j.string_of_t_list (List.map Building.create_parsed_building_from_unbound buildings_list)

(* Settings config *)

let create_settings_from_file file config_name =
  let t = Ag_util.Json.from_file Settings_j.read_t file in
  let tr = check_error file Settings_v.validate_t [] t in
  ConfigLog.infof "%s[loaded] settings : %s" config_name file;
  tr

let create_engine_settings_from_file file config_name =
  let t = Ag_util.Json.from_file Settings_engine_j.read_t file in
  let tr = check_error file Settings_engine_v.validate_t [] t in
  ConfigLog.infof "%s[loaded] settings_engine : %s" config_name file;
  tr

let create_interface_settings_from_file file config_name =
  let t = Ag_util.Json.from_file Settings_interface_j.read_t file in
  let tr = check_error file Settings_interface_v.validate_t [] t in
  ConfigLog.infof "%s[loaded] settings_interface : %s" config_name file;
  tr

let write_settings_in_file file settings config_name =
  Ag_util.Json.to_file Settings_j.write_t file settings;
  ConfigLog.infof "%s[saved] settings : %s" config_name file

let write_engine_settings_in_file file settings config_name =
  Ag_util.Json.to_file Settings_engine_j.write_t file settings;
  ConfigLog.infof "%s[saved] settings_engine : %s" config_name file

let write_interface_settings_in_file file settings config_name =
  Ag_util.Json.to_file Settings_interface_j.write_t file settings;
  ConfigLog.infof "%s[saved] settings_interface : %s" config_name file

let create_settings_from_string config_name s =
  let t = Settings_j.t_of_string s in
  let tr = check_error "settings" Settings_v.validate_t [] t in
  ConfigLog.infof "%s[loaded] settings" config_name;
  tr

let create_engine_settings_from_string config_name s =
  let t = Settings_engine_j.t_of_string s in
  let tr = check_error "engine_setings" Settings_engine_v.validate_t [] t in
  ConfigLog.infof "%s[loaded] settings_engine" config_name;
  tr

let create_interface_settings_from_string config_name s =
  let t = Settings_interface_j.t_of_string s in
  let tr = check_error "interface_settings" Settings_interface_v.validate_t [] t in
  ConfigLog.infof "%s[loaded] settings_interface" config_name;
  tr

let string_of_settings settings =
  Settings_j.string_of_t settings

let string_of_engine_settings settings =
  Settings_engine_j.string_of_t settings

let string_of_interface_settings settings =
  Settings_interface_j.string_of_t settings


(* default parameters *)

let base = Utils.base_path () ^ "config/"

let default_config_files =
  (base ^ "tiles.json",
   base ^ "units.json",
   base ^ "buildings.json",
   base ^ "settings.json",
   base ^ "settings_default.json")

let default_engine_settings_files =
  (base ^ "settings_engine.json",
   base ^ "settings_engine_default.json")

let default_interface_settings_files =
  (base ^ "settings_interface.json",
   base ^ "settings_interface_default.json")

let default_files =
  let (x1,x2,x22,x3,x4) = default_config_files in
  let (x5,x6) = default_engine_settings_files in
  let (x7,x8) = default_interface_settings_files in
  (x1,x2,x22,x3,x4,x5,x6,x7,x8)

(* config class *)
class t =
object (self)

  val mutable config_name = ""
  method set_config_name s = config_name <- s

  val mutable tiles_config = ""
  val mutable units_config = ""
  val mutable buildings_config = ""
  val mutable settings_temp = ""
  val mutable settings_default = ""
  val mutable interface_settings_temp = ""
  val mutable interface_settings_default = ""
  val mutable engine_settings_temp = ""
  val mutable engine_settings_default = ""

  val mutable t_list = (None: (Tile.t list) option)
  val mutable u_list = (None: (Unit.unbound_t list) option)
  val mutable b_list = (None: (Building.unbound_t list) option)
  
  val mutable s = (None: Settings_t.t option)
  val mutable engine_s = (None: Settings_engine_t.t option)
  val mutable interface_s = (None: Settings_interface_t.t option)

  val mutable safe_s = (None: Settings_t.t option)
  val mutable safe_engine_s = (None: Settings_engine_t.t option)
  val mutable safe_interface_s = (None: Settings_interface_t.t option)

  method private update_safe_s = safe_s <- match s with | None -> None | Some ss -> Some {ss with Settings_t.none=()}
  method private revert_s = s <- match safe_s with | None -> None | Some ss -> Some {ss with Settings_t.none=()}
  method private update_safe_engine_s = safe_engine_s <- match engine_s with | None -> None | Some ss -> Some {ss with Settings_engine_t.none=()}
  method private revert_engine_s = engine_s <- match safe_engine_s with | None -> None | Some ss -> Some {ss with Settings_engine_t.none=()}
  method private update_safe_interface_s = safe_interface_s <- match interface_s with | None -> None | Some ss -> Some {ss with Settings_interface_t.none=()}
  method private revert_interface_s = interface_s <- match safe_interface_s with | None -> None | Some ss -> Some {ss with Settings_interface_t.none=()}

  method private available_settings = if Sys.file_exists settings_temp then settings_temp else settings_default
  method private available_engine_settings = if Sys.file_exists engine_settings_temp then engine_settings_temp else engine_settings_default
  method private available_interface_settings = if Sys.file_exists interface_settings_temp then interface_settings_temp else interface_settings_default

  method private init_names tiles_file units_file buildings_file
                    settings_temp_file settings_default_file
                    engine_settings_temp_file engine_settings_default_file
                    interface_settings_temp_file interface_settings_default_file =
    let aux a b = if a <> "" then (if Sys.file_exists a then a else (ConfigLog.errorf "%s[missing] %s" config_name a; b)) else b in
    tiles_config <- aux tiles_file tiles_config;
    units_config <- aux units_file units_config;
    buildings_config <- aux buildings_file buildings_config;
    if settings_temp_file <> "" then settings_temp <- settings_temp_file;
    settings_default <- aux settings_default_file settings_default;
    if engine_settings_temp_file <> "" then engine_settings_temp <- engine_settings_temp_file;
    engine_settings_default <- aux engine_settings_default_file engine_settings_default;
    if interface_settings_temp_file <> "" then interface_settings_temp <- interface_settings_temp_file;
    interface_settings_default <- aux interface_settings_default_file interface_settings_default;
    self#reload_all

  method private init_all (x1,x2,x22,x3,x4,x5,x6,x7,x8) =
    self#init_names x1 x2 x22 x3 x4 x5 x6 x7 x8
  method init (tiles_file, units_file, buildings_file, settings_temp_file, settings_default_file) =
    self#init_names
          tiles_file units_file buildings_file
          settings_temp_file settings_default_file
          "" ""
          "" ""
  method init_engine (engine_settings_temp_file, engine_settings_default_file) =
    self#init_names
          "" "" ""
          "" ""
          engine_settings_temp_file engine_settings_default_file
          "" ""
  method init_interface (interface_settings_temp_file, interface_settings_default_file) =
    self#init_names
            "" "" ""
            "" ""
            "" ""
            interface_settings_temp_file interface_settings_default_file
  method init_default = self#init_all default_files


  method tiles_list = match t_list with
    | Some a -> a
    | None -> ConfigLog.fatalf "%s[missing] tiles" config_name;raise (Missing_config "no valid tiles file loaded so far, did you call init?")
  method unbound_units_list = match u_list with
    | Some a -> a
    | None -> ConfigLog.fatalf "%s[missing] units" config_name;raise (Missing_config "no valid units file loaded so far, did you call init?")
  method unbound_buildings_list = match b_list with
    | Some a -> a
    | None -> ConfigLog.fatalf "%s[missing] buildings" config_name;raise (Missing_config "no valid buildings file loaded so far, did you call init?")
  method private settings_unsafe = match s with
    | Some a -> a
    | None -> ConfigLog.fatalf "%s[missing] settings" config_name;raise (Missing_config "no valid settings file loaded so far, did you call init?")
  method private settings_engine_unsafe = match engine_s with
    | Some a -> a
    | None -> ConfigLog.fatalf "%s[missing] settings_engine" config_name;raise (Missing_config "no valid engine settings file loaded so far, did you call init_engine?")
  method private settings_interface_unsafe = match interface_s with
    | Some a -> a
    | None -> ConfigLog.fatalf "%s[missing] settings_interface" config_name;raise (Missing_config "no valid interface settings file loaded so far, did you call init_interface?")
  method settings = self#fix_settings;self#settings_unsafe
  method settings_engine = self#fix_settings_engine;self#settings_engine_unsafe
  method settings_interface = self#fix_settings_interface;self#settings_interface_unsafe

  method tile config_name = List.find (fun t -> Tile.get_name t = config_name) self#tiles_list
  method unbound_unit config_name = List.find (fun uni -> uni#name = config_name) self#unbound_units_list
  method price unit_name = (self#unbound_unit unit_name)#price
  method unbound_building config_name = List.find (fun uni -> uni#name = config_name) self#unbound_buildings_list

  method private load_settings str =
    if str <> "" then
      (s <- ( try Some (create_settings_from_file str config_name) with Config_error (msg) -> ConfigLog.errorf "%s" msg;None );
      if s <> None then self#update_safe_s)
  method private load_settings_engine str =
    if str <> "" then
      (engine_s <- ( try Some (create_engine_settings_from_file str config_name) with Config_error (msg) -> ConfigLog.errorf "%s" msg;None );
      if engine_s <> None then self#update_safe_engine_s)
  method private load_settings_interface str =
    if str <> "" then
      (interface_s <- ( try Some (create_interface_settings_from_file str config_name) with Config_error (msg) -> ConfigLog.errorf "%s" msg;None );
      if interface_s <> None then self#update_safe_interface_s)

  method reload_settings = self#load_settings self#available_settings
  method reload_settings_engine = self#load_settings_engine self#available_engine_settings
  method reload_settings_interface = self#load_settings_interface self#available_interface_settings
  method reload_all =
    if tiles_config <> "" then t_list <- ( try Some (create_tiles_list_from_file tiles_config config_name) with Config_error (msg) -> ConfigLog.errorf "%s" msg;None );
    if units_config <> "" then u_list <- ( try Some (create_unbound_units_list_from_file units_config config_name) with Config_error (msg) -> ConfigLog.errorf "%s" msg;None );
    if buildings_config <> "" then b_list <- ( try Some (create_unbound_buildings_list_from_file buildings_config config_name) with Config_error (msg) -> ConfigLog.errorf "%s" msg;None );
    self#reload_settings;
    self#reload_settings_engine;
    self#reload_settings_interface

  method reset_settings = self#load_settings settings_default
  method reset_settings_engine = self#load_settings_engine engine_settings_default
  method reset_settings_interface = self#load_settings_interface interface_settings_default
  method reset_all =
    self#reset_settings;
    self#reset_settings_engine;
    self#reset_settings_interface

  method load_from_strings tiles_s unbound_units_s unbound_buildings_s settings_s =
    t_list <- ( try Some (create_tiles_list_from_string tiles_s config_name) with Config_error (msg) -> ConfigLog.errorf "%s" msg;None );
    u_list <- ( try Some (create_unbound_units_list_from_string unbound_units_s config_name) with Config_error (msg) -> ConfigLog.errorf "%s" msg;None );
    b_list <- ( try Some (create_unbound_buildings_list_from_string unbound_buildings_s config_name) with Config_error (msg) -> ConfigLog.errorf "%s" msg;None );
    s <- ( try Some (create_settings_from_string settings_s config_name) with Config_error (msg) -> ConfigLog.errorf "%s" msg;None )

  method string_of_tiles_list = string_of_tiles_list self#tiles_list
  method string_of_unbound_units_list = string_of_unbound_units_list self#unbound_units_list
  method string_of_unbound_buildings_list = string_of_unbound_buildings_list self#unbound_buildings_list
  method string_of_settings = string_of_settings self#settings

  method save_settings = self#fix_settings;
    if s <> None then write_settings_in_file settings_temp self#settings_unsafe config_name
  method save_settings_engine = self#fix_settings_engine;
    if engine_s <> None then write_engine_settings_in_file engine_settings_temp self#settings_engine_unsafe config_name
  method save_settings_interface = self#fix_settings_interface;
    if interface_s <> None then write_interface_settings_in_file interface_settings_temp self#settings_interface_unsafe config_name
  method save_all =
    self#save_settings;
    self#save_settings_engine;
    self#save_settings_interface

  method private check_settings =
    if s <> None then
      (let _ = check_error "settings" Settings_v.validate_t [] self#settings_unsafe in
      self#update_safe_s)
  method private check_settings_engine =
    if engine_s <> None then
      (let _ = check_error "settings_engine" Settings_engine_v.validate_t [] self#settings_engine_unsafe in
      self#update_safe_engine_s)
  method private check_settings_interface =
    if interface_s <> None then
      (let _ = check_error "settings_interface" Settings_interface_v.validate_t [] self#settings_interface_unsafe in
      self#update_safe_interface_s)

  method private fix_settings =
    (try self#check_settings
    with | Config_error (msg) ->
      self#revert_s;
      ConfigLog.errorf "%s" msg;
      ConfigLog.infof "%s[reverted] settings" config_name)
  method private fix_settings_engine =
    (try self#check_settings_engine
    with | Config_error (msg) ->
      self#revert_engine_s;
      ConfigLog.errorf "%s" msg;
      ConfigLog.infof "%s[reverted] settings_engine" config_name)
  method private fix_settings_interface =
    (try self#check_settings_interface
    with | Config_error (msg) ->
      self#revert_interface_s;
      ConfigLog.errorf "%s" msg;
      ConfigLog.infof "%s[reverted] settings_interface" config_name)

  method private char_of_tile_off tile offset =
    let tiles = self#tiles_list in
    if List.length tiles > 256 then failwith("more than 255 tiles in config, char_of_tile can't be applied") else
    let rec mempos e = function
    | n,[] -> failwith("tile not in config")
    | n,p::q when Tile.get_name p = Tile.get_name e -> n
    | n,p::q -> mempos e (n+1,q)
    in
    char_of_int((mempos tile (offset,tiles)) mod 256)
  method char_of_tile tile = self#char_of_tile_off tile self#settings.serializer_offset

  method private tile_of_char_off c offset =
    let tiles = self#tiles_list in
    if List.length tiles > 256 then failwith("more than 255 tiles in config, char_of_tile can't be applied") else
    List.nth tiles (((int_of_char c - offset) +256) mod 256)
  method tile_of_char c = self#tile_of_char_off c self#settings.serializer_offset

  method private char_of_unbound_unit_off (u:Unit.unbound_t) offset =
    let units = self#unbound_units_list in
    if List.length units > 256 then failwith("more than 255 units in config, char_of_unbound_unit can't be applied") else
    let rec mempos e = function
    | n,[] -> failwith("unit not in config")
    | n,p::q when p#name = e#name -> n
    | n,p::q -> mempos e (n+1,q)
    in
    char_of_int((mempos u (offset,units)) mod 256)
  method char_of_unbound_unit u = self#char_of_unbound_unit_off u self#settings.serializer_offset

  method private unbound_unit_of_char_off c offset =
    let units = self#unbound_units_list in
    if List.length units > 256 then failwith("more than 255 units in config, char_of_unbound_unit can't be applied") else
    List.nth units (((int_of_char c - offset) +256) mod 256)
  method unbound_unit_of_char c = self#unbound_unit_of_char_off c self#settings.serializer_offset
  
  method private char_of_unbound_building_off (u:Building.unbound_t) offset =
    let buildings = self#unbound_buildings_list in
    if List.length buildings > 256 then failwith("more than 255 buildings in config, char_of_unbound_building can't be applied") else
    let rec mempos e = function
    | n,[] -> failwith("building not in config")
    | n,p::q when p#name = e#name -> n
    | n,p::q -> mempos e (n+1,q)
    in
    char_of_int((mempos u (offset,buildings)) mod 256)
  method char_of_unbound_building u = self#char_of_unbound_building_off u self#settings.serializer_offset

  method private unbound_building_of_char_off c offset =
    let buildings = self#unbound_buildings_list in
    if List.length buildings > 256 then failwith("more than 255 buildings in config, char_of_unbound_building can't be applied") else
    List.nth buildings (((int_of_char c - offset) +256) mod 256)
  method unbound_building_of_char c = self#unbound_building_of_char_off c self#settings.serializer_offset

  method string_of_unit (u:Unit.t) =
    let s0 = let s = "0" in s.[0] <- (self#char_of_unbound_unit (u:> Unit.unbound_t));s in
    let (s1,s2) = let (a,b) = Position.topair u#position in (string_of_int a,string_of_int b) in
    let s3 = string_of_int u#player_id in
    let s4 = string_of_int u#get_id in
    let s5 = string_of_int u#hp in
    let s6 = if u#has_played then "0" else "1" in
    s0^","^s1^","^s2^","^s3^","^s4^","^s5^","^s6

  method unit_of_string s =
    let s_size = String.length s in
    let rec to_list_aux = function
    | n when n = s_size -> []
    | n when s.[n] < '0' || s.[n] > '9' -> n::(to_list_aux (n+1))
    | n -> to_list_aux (n+1)
    in
    let rec to_list = function
    | [] -> assert false
    | [p] -> let ss = String.sub s (p+1) 1 in [ss]
    | p::q::l -> let ss = String.sub s (p+1) (q-p-1) in ss::(to_list (q::l))
    in
    let l = to_list ((-1)::1::(to_list_aux 2)) in
    if List.length l <> 7 then failwith "not a valid unit" else
    let al = Array.of_list l in
    let (u,p,pid,id,hp,h_p) = (self#unbound_unit_of_char al.(0).[0], Position.create(int_of_string al.(1),int_of_string al.(2)), int_of_string al.(3), int_of_string al.(4), int_of_string al.(5), al.(6)="1") in
    Unit.bind_extended u p pid id hp h_p
    
  method string_of_building (u:Building.t) =
    let s0 = let s = "0" in s.[0] <- (self#char_of_unbound_building (u:> Building.unbound_t));s in
    let (s1,s2) = let (a,b) = Position.topair u#position in (string_of_int a,string_of_int b) in
    let s3 = match u#player_id with None -> "None" | Some id -> string_of_int id in
    let s4 = string_of_int u#get_id in
    s0^","^s1^","^s2^","^s3^","^s4

  method building_of_string s =
    let s_size = String.length s in
    let rec to_list_aux = function
    | n when n = s_size -> []
    | n when s.[n] < '0' || s.[n] > '9' -> n::(to_list_aux (n+1))
    | n -> to_list_aux (n+1)
    in
    let rec to_list = function
    | [] -> assert false
    | [p] -> let ss = String.sub s (p+1) 1 in [ss]
    | p::q::l -> let ss = String.sub s (p+1) (q-p-1) in ss::(to_list (q::l))
    in
    let l = to_list ((-1)::1::(to_list_aux 2)) in
    if List.length l <> 5 then failwith "not a valid building" else
    let al = Array.of_list l in
    let (u,p,pid,id) = (self#unbound_building_of_char al.(0).[0], Position.create(int_of_string al.(1),int_of_string al.(2)), (match al.(3) with | "None" -> None | id -> Some (int_of_string id)), int_of_string al.(4)) in
    Building.bind_extended u p pid id

  method private string_of_battlefield_off m off2 =
    let (w,h) = Battlefield.size m in
    let s = Utils.init_string
      (w*h)
      (fun i ->
        let pos = Position.create (i/w,i mod w) in
        self#char_of_tile (Battlefield.get_tile m pos)
      ) in
    let compress s =
      let rec aux = function
      | [],i when 0<=i && i < String.length s -> aux([(s.[i],1)],i+1)
      | (a,b)::q,i when 0<=i && i < String.length s ->
        if a = s.[i] && b<255 then
          aux((a,b+1)::q,i+1)
        else
          aux((s.[i],1)::(a,b)::q,i+1)
      |l,i-> l
      in
      let li = List.rev (aux ([],0)) in
      let ss = Utils.init_string (2*List.length li) (fun i -> '?') in
      let rec list_to_string n = function
      | []->ss
      | (a,b)::q ->
        ss.[2*n] <- a;
        ss.[2*n+1] <- (char_of_int ((b+off2) mod 256));
        list_to_string (n+1) q
      in
      list_to_string 0 li
    in
    compress s
  method string_of_battlefield m = self#string_of_battlefield_off m self#settings.string_compression_offset

  method private battlefield_of_string_off w h s_short off2 =
    let decompress s =
      let rec string_to_list = function
      |i when 2*i+1 < String.length s -> (s.[2*i],let j = int_of_char s.[2*i+1] - off2 in if(j>=0) then j else j+256)::(string_to_list (i+1))
      |i -> []
      in
      let li = string_to_list 0 in
      let size_new_string = List.fold_left (fun t e -> snd e + t) 0 li in
      let ss = Utils.init_string size_new_string (fun i -> '?') in
      let rec fill_string p = function
      | [] -> ss
      | (a,b)::q ->
        for i = 0 to b-1 do
          ss.[p+i] <- a;
        done;
        fill_string (p+b) q
      in
      fill_string 0 li
    in
    let s = decompress s_short in
    let m = Battlefield.create w h (self#tile "blank") in
    Battlefield.tile_iteri
      (fun p _ ->
        let i = let x,y = Position.topair p in x*w+y in
        Battlefield.set_tile m p (self#tile_of_char s.[i])
      ) m;
    m
  method battlefield_of_string s = self#battlefield_of_string_off self#settings.map_width self#settings.map_height s self#settings.string_compression_offset

end

let config = new t

let _ = config#init_default


