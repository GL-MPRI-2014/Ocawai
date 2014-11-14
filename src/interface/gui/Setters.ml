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

  inherit modal

  method position = pos

  method draw (target : OcsfmlGraphics.render_window) =

    let bg_color =
      if self#holds_focus then Color.rgb 222 222 222
      else if has_focus then Color.rgb 237 240 242
      else Color.white
    in
    new rectangle_shape ~fill_color:bg_color
      ~size:(setter_width,setter_height) ~position:self#position
      ~origin:(setter_width/.2.,setter_height/.2.)
      ()
    |> target#draw ;

    rect_print
      target name font (Color.rgb 64 64 64) (Pix 20) (Pix 2) Left {
        left = fst self#position +. 20. -. 400. ;
        top = snd self#position +. 10. -. 20. ;
        width = setter_width -. setting_width -. 4. ;
        height = setter_height }

end

class ['a] slider pos (f : int -> 'a) name = object(self)

  inherit setter pos name as super_set

  val slider_h = 2.
  val slider_w = setting_width -. 20.

  val cursor_r = 10.

  val mutable percentage = 50

  method value = f percentage

  method draw (target : OcsfmlGraphics.render_window) =

    super_set#draw target ;
    (* First we have a line for the slider *)
    let color = Color.rgb 57 131 204 in
    new rectangle_shape ~fill_color:color
      ~size:(slider_w,slider_h)
      ~origin:(slider_w /. 2., slider_h /. 2.)
      ~position:(addf2D self#position ((setter_width -. setting_width) /. 2., 0.))
      ()
    |> target#draw ;
    (* Then we draw the cursor *)
    let offset = (slider_w /. 100.) *. (float_of_int (percentage - 50)) in
    let position =
      addf2D self#position
             ((setter_width -. setting_width) /. 2. +. offset, 0.) in
    new circle_shape
      ~fill_color: color
      ~radius: cursor_r
      ~origin: (cursor_r, cursor_r)
      ~position
      ~point_count: 100
      ()
    |> target#draw


  method action =
    holds_focus <- true

  method handle_key = OcsfmlWindow.KeyCode.(function
    | Left -> ()
    | Right -> ()
    | Return -> holds_focus <- false
    | _ -> ()
  )

end
