(** Config files and settings gestion module*)

(** default tiles, units, settings,and settings_default paths*)
val default_config_files : string * string * string * string

(** default settings_engine,and settings_engine_default paths*)
val default_engine_settings_files : string * string

(** default settings_interface,and settings_interface_default paths*)
val default_interface_settings_files : string * string

(** config gestion class *)
class t : 
object

  (** Initialize the tiles, units, settings,and settings_default files*)
  method init : string * string * string * string -> unit
  (** Initialize the engine settings files *)
  method init_engine : string * string -> unit
  (** Initialize the interface settings files *)
  method init_interface : string * string -> unit
  (** Initialize everything by default*)
  method init_default : unit
  
  (** Tiles list obtained by reading the json config file *)
  method tiles_list : Tile.t list
  method tile : string -> Tile.t
  
  (** Units.unbound_t list obtained by reading the json config file*)
  method unbound_units_list : Unit.unbound_t list
  method unbound_unit : string -> Unit.unbound_t
  
  (** settings obtained by reading the settings json file if present, settings-default if not*)
  method settings : Settings_t.t
  (** engine settings obtained by reading the settings json file if present, settings-default if not*)
  method settings_engine : Settings_engine_t.t
  (** interface settings obtained by reading the settings json file if present, settings-default if not*)
  method settings_interface : Settings_interface_t.t
  
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
  
  (** Save currents settings in settings file, persistent until make clean *)
  method save_settings : unit
  (** Save currents settings_engine in settings_engine file, persistent until make clean *)
  method save_settings_engine : unit
  (** Save currents settings_interface in settings_interface file, persistent until make clean *)
  method save_settings_interface : unit
  (** Save currents settings in settings files, persistents until make clean *)
  method save_all : unit
  
  (** check settings validity*)
  method check_all : bool
  
end

val config : t

