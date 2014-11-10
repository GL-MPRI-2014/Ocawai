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

  method handle_event e =

    OcsfmlWindow.Event.(
      match e with
        | KeyPressed { code = OcsfmlWindow.KeyCode.Back ; _ } ->
            manager#pop
        | _ -> ()
    )

  method render window =

    super#render window ;

    Interpolators.update ();

    let color = Color.rgb 221 224 234 in
    window#clear ~color ();

    let (w,h) = foi2D window#get_size in

    rect_print
      window "SETTINGS" font Color.black (Pix 70) (Pix 10) Left
      { left = 10. ; top = 10. ; width = w -. 20. ; height = 100. };

    window#display

  initializer
    if not (font#load_from_file "resources/fonts/Roboto-Black.ttf")
    then failwith "Couldn't load the font here";
    ignore(Interpolators.new_sine_ip
      self#set_alpha 2. 0.4 0.6);

  method destroy =
    ()

end
