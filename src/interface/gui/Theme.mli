open OcsfmlGraphics

type t = {
  bar_color         : Color.t;
  default_color     : Color.t;
  highlight_color   : Color.t;
  hover_color       : Color.t;
  active_color      : Color.t}

val blue_theme : t

val yellow_theme : t

val red_theme : t
