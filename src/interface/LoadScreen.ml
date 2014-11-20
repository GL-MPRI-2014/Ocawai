open OcsfmlGraphics
open Utils
open GuiTools

open Manager

class state = object(self)

  inherit State.state as super

  val font = new font `None

  val mutable text_alpha = 1.

  method private set_alpha a =
    text_alpha <- a

  method render window =

    Interpolators.update ();

    let color = Color.rgb 19 42 69 in
    window#clear ~color ();

    let (w,h) = foi2D window#get_size in

    let text_color =
      Color.rgba 255 255 255 (int_of_float (255. *. text_alpha)) in
    rect_print
      window "LOADING" font text_color (Pix 120) (Pix 10) Center
      { left = 0. ; top = h /. 2. -. 100. ; width = w ; height = 100. };

    window#display

  initializer
    if not (font#load_from_file "resources/fonts/Roboto-Black.ttf")
    then failwith "Couldn't load the font here";
    ignore(Interpolators.new_sine_ip
      self#set_alpha 2. 0.4 0.6);

  method destroy =
    ()

end
