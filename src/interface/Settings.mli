(** Module to handle settings *)

(** Screen mode *)
type screen_mode = Fullscreen | Windowed

(** Object to hold settings *)
val settings = <

  set_mode : screen_mode -> unit ;
  mode : screen_mode

>
