(** Config files and settings gestion module*)

(* global config files and setting gestion, internally instanciates a Config.t object *)

(** Initialize the engine settings files *)
val init_engine : unit -> unit

(** Initialize the interface settings files *)
val init_interface : unit -> unit

(** Tiles list obtained by reading the json config file *)
val tiles_list : unit -> Tile.t list
val tile : string -> Tile.t

(** Units.unbound_t list obtained by reading the json config file*)
val unbound_units_list : unit -> Unit.unbound_t list
val unbound_unit : string -> Unit.unbound_t

(** settings obtained by reading the settings json file if present, settings-default if not*)
val settings : unit -> Settings_t.t

(** settings obtained by reading the settings json file if present, settings-default if not*)
val settings_engine : unit -> Settings_engine_t.t

(** settings obtained by reading the settings json file if present, settings-default if not*)
val settings_interface : unit -> Settings_interface_t.t

(** Parse everything again *)
val reload : unit -> unit

(** Load the settings from the settings-default file*)
val reset_to_default : unit -> unit

(** Save currents settings in a settings file, persistent until make clean *)
val save_settings : unit -> unit

(* local config gestion *)

(** default tiles, units, settings,and settings_default paths*)
val default_config_files : string * string * string * string

(** default tiles, units, settings_engine,and settings_engine_default paths*)
val default_engine_settings_files : string * string

(** default tiles, units, settings_interface,and settings_interface_default paths*)
val default_interface_settings_files : string * string

(** config gestion class *) (* used internally above, can be used to create local configs (for tests?)*)
class t : 
object
  method init : string * string * string * string -> unit
  method init_engine : string * string -> unit
  method init_interface : string * string -> unit
  method tiles_list : Tile.t list
  method tile : string -> Tile.t
  method unbound_units_list : Unit.unbound_t list
  method unbound_unit : string -> Unit.unbound_t
  method settings : Settings_t.t
  method settings_engine : Settings_engine_t.t
  method settings_interface : Settings_interface_t.t
  method reload : unit
  method reset_to_default : unit
  method save_settings : unit
end

