(** Definition of themes for widgets *)

open OcsfmlGraphics

(** The type of a theme *)
type t = {
  bar_color         : Color.t;
  default_color     : Color.t;
  highlight_color   : Color.t;
  border_color      : Color.t;
  active_color      : Color.t}

val blue_theme : t

val yellow_theme : t

val red_theme : t
