(** Module to handle settings *)

(** Screen mode *)
type screen_mode = Fullscreen | Windowed

(** Object to hold settings *)
val settings : <

  set_mode : screen_mode -> unit ;
  mode : screen_mode ;

  set_move_speed : float -> unit ;
  move_speed : float ;

  set_zoom_speed : float -> unit ;
  zoom_speed : float

>
