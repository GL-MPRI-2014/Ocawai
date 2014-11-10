(** Module to handle settings *)

(** Screen mode *)
type screen_mode = Fullscreen | Windowed

(** Object to hold settings *)
val settings : <

  (** Sets the screen mode (TODO) *)
  set_mode : screen_mode -> unit ;
  mode : screen_mode ;

  (** Speed of the cursor (in game) *)
  set_cursor_speed : float -> unit ;
  cursor_speed : float ;

  (** Speed of the zoom *)
  set_zoom_speed : float -> unit ;
  zoom_speed : float

>
