open OcsfmlGraphics

open Settings
open Home
open GuiTools
open Utils

let font = new font (`File "resources/fonts/Roboto-Regular.ttf")

let setter_width = 800.
let setting_width = 300.
let setter_height = 40.

class virtual setter pos name = object(self)

  inherit actionnable

  method position = pos

  method draw (target : OcsfmlGraphics.render_window) =

    let bg_color = if has_focus then Color.blue else Color.white in
    new rectangle_shape ~fill_color:bg_color
      ~size:(setter_width,setter_height) ~position:self#position
      ~origin:(400.,20.)
      ()
    |> target#draw ;

    rect_print
      target name font Color.black (Pix 30) (Pix 2) Left {
        left = fst self#position +. 2. -. 400. ;
        top = snd self#position +. 4. -. 20. ;
        width = setter_width -. setting_width -. 4. ;
        height = setter_height }

end

class ['a] slider pos (f : int -> 'a) name = object(self)

  inherit setter pos name as super_set
  inherit modal

  val mutable percentage = 50

  method value = f percentage

  method draw (target : OcsfmlGraphics.render_window) =

    super_set#draw target

  method action = ()

  method handle_key key = ()

end
