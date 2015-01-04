(** Config files and settings gestion module
    @author dbusatto*)

(** raised when trying to acces a data never loaded before*)
exception Missing_config of string

(** config gestion class *)
class t : 
object

(** {6 Initializers} *)

  method set_config_name : string -> unit
  
  (** Initialize the tiles, units, unbound_buildings settings,and settings_default files*)
  method init : string * string * string * string * string -> unit
  
  (** Initialize the engine settings files*)
  method init_engine : string * string -> unit
  
  (** Initialize the interface settings files*)
  method init_interface : string * string -> unit
  
  (** Initialize everything by default*)
  method init_default : unit
  
(** {6 Internal data access} *)
  
  (** Tiles list obtained by reading the json config file *)
  method tiles_list : Tile.t list
  
  (** Create a tile from its name by searching in tile_list*)
  method tile : string -> Tile.t
  
  
  (** Units.unbound_t list obtained by reading the json config file*)
  method unbound_units_list : Unit.unbound_t list
  
  (** Create an unbound_unit from its name by searching in unbond_units_list *)
  method unbound_unit : string -> Unit.unbound_t
  
  (** Return the price of a given unit name *)
  method price : string -> int
  
  
  (** Tiles list obtained by reading the json config file *)
  method unbound_buildings_list : Building.unbound_t list
  
  (** Create a tile from its name by searching in tile_list*)
  method unbound_building : string -> Building.unbound_t
  
  
  (** settings obtained by reading the settings json file if present, settings-default if not*)
  method settings : Settings_t.t
  
  (** engine settings obtained by reading the settings json file if present, settings-default if not*)
  method settings_engine : Settings_engine_t.t
  
  (** interface settings obtained by reading the settings json file if present, settings-default if not*)
  method settings_interface : Settings_interface_t.t
  
(** {6 Loading} *)
  
  (** Parse settings again *)
  method reload_settings : unit
  
  (** Parse settings_engine again *)
  method reload_settings_engine : unit
  
  (** Parse settings_interface again *)
  method reload_settings_interface : unit
  
  (** Parse everything again *)
  method reload_all : unit
  
  
  (** Load the settings from the settings_default file*)
  method reset_settings : unit
  
  (** Load the settings_engine from the settings_engine_default file*)
  method reset_settings_engine : unit
  
  (** Load the settings_interface from the settings_interface_default file*)
  method reset_settings_interface : unit
  
  (** Load the settings from the settings_default files*)
  method reset_all : unit
  
  
  (** load from tiles, units unbound_buildings and settings serialized strings *)
  method load_from_strings : string -> string -> string -> string -> unit
  
  
  (** Save currents settings in settings file, persistent until make clean*)
  method save_settings : unit
  
  (** Save currents settings_engine in settings_engine file, persistent until make clean*)
  method save_settings_engine : unit
  
  (** Save currents settings_interface in settings_interface file, persistent until make clean*)
  method save_settings_interface : unit
  
  (** Save currents settings in settings files, persistents until make clean*)
  method save_all : unit
  
(** {6 Serializers} *)
  
  method string_of_tiles_list : string
  method string_of_unbound_units_list : string
  method string_of_unbound_buildings_list : string
  method string_of_settings : string
  
  method char_of_tile : Tile.t -> char
  method tile_of_char : char -> Tile.t
  
  method char_of_unbound_building : Building.unbound_t -> char
  method unbound_building_of_char : char -> Building.unbound_t
  
  method char_of_unbound_unit : Unit.unbound_t -> char
  method unbound_unit_of_char : char -> Unit.unbound_t
  
  method string_of_unit : Unit.t -> string
  method unit_of_string : string -> Unit.t
  
  method string_of_battlefield : Battlefield.t -> string
  method battlefield_of_string : string -> Battlefield.t
  
end

(** Global instance of {!t}*)
val config : t

(** {6 Default files} *)

(** default tiles, units, unbound_buildingd, settings,and settings_default paths*)
val default_config_files : string * string * string * string * string

(** default settings_engine,and settings_engine_default paths*)
val default_engine_settings_files : string * string

(** default settings_interface,and settings_interface_default paths*)
val default_interface_settings_files : string * string

