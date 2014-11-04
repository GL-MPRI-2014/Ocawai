open OcsfmlGraphics

type t = {
  bar_color         : Color.t;
  default_color     : Color.t;
  highlight_color   : Color.t;
  hover_color       : Color.t;
  active_color      : Color.t}


let blue_theme = 
  {bar_color       = Color.rgb 56 78 147;
   default_color   = Color.rgb 186 217 244;
   highlight_color = Color.rgb 139 180 228;
   hover_color     = Color.rgb 215 233 249;
   active_color    = Color.rgb 30 115 189}
