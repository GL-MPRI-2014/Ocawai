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
  method init_default : unit -> unit
  
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
  
  (** Parse everything again *)
  method reload : unit
  
  (** Load the settings from the settings_default files*)
  method reset_to_default : unit
  
  (** Save currents settings in settings files, persistents until make clean *)
  method save_settings : unit
  
end



