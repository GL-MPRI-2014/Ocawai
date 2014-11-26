(** Config files and settings gestion module*)

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

