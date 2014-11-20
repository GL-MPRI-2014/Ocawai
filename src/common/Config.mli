(** Config and settings gestion module*)

(** default tiles, units, settings,and settings-default paths*)
val default_config_files : string * string * string *     string

(*        tiles,   units,   settings,settings-default paths*)
class t : string * string * string * string -> 
object
  (** Tiles list obtained by reading the json config file *)
  method tiles_list : Tile.t list
  method tile : string -> Tile.t
  
  (** Units.unbound_t list obtained by reading the json config file*)
  method unbound_units_list : Unit.unbound_t list
  method unbound_unit : string -> Unit.unbound_t
  
  (** settings obtained by reading the settings json file if present, settings-default if not*)
  method settings : Settings_t.t
  
  (** Parse the tiles config file again*)
  method reload_tiles : unit
  
  (** Parse the units config file again*)
  method reload_units : unit
  
  (** Parse the settings config file again if present, settings-default if not*)
  method reload_settings : unit
  
  (** Parse everything again *)
  method reload : unit
  
  (** Load the settings from the settings-default file*)
  method reset_settings : unit
  
  (** Save currents settings in a settings file, persistent until make clean *)
  method write_settings : unit
end

