open OcsfmlGraphics

type t = {
  bar_color         : Color.t;
  default_color     : Color.t;
  highlight_color   : Color.t;
  border_color      : Color.t;
  active_color      : Color.t}


let blue_theme = 
  {bar_color       = Color.rgb 56 78 147;
   default_color   = Color.rgb 186 217 244;
   highlight_color = Color.rgb 139 180 228;
   border_color    = Color.rgb 120 160 200;
   active_color    = Color.rgb 30 115 189}


let yellow_theme =
  {bar_color       = Color.rgb 90 90 10;
   default_color   = Color.rgb 190 190 130;
   highlight_color = Color.rgb 155 155 110;
   border_color    = Color.rgb 130 130 100;
   active_color    = Color.rgb 120 120 0}

let red_theme =
  {bar_color       = Color.rgb 100 10 10;
   default_color   = Color.rgb 200 120 120;
   highlight_color = Color.rgb 160 110 110;
   border_color    = Color.rgb 150 100 100;
   active_color    = Color.rgb 130 0 0}
