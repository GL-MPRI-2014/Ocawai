open OcsfmlGraphics
open Utils
open GuiTools

open Manager

class main_menu = object(self)

  inherit State.state as super

  val font = new font `None

  val splash_font = new font `None

  val mutable text_alpha = 1.

  val mutable splash_size = 1.

  method private set_alpha a =
    text_alpha <- a

  method private set_splash_size s =
    splash_size <- s

  method handle_event e =

    OcsfmlWindow.Event.(
      match e with
        | KeyPressed { code = _ ; _ } ->
            (new Game.game :> State.state) |> manager#push
        | _ -> ()
    )

  method render window =

    super#render window ;

    Interpolators.update ();

    let color = Color.rgb 221 224 234 in
    window#clear ~color ();

    (* Let's hardcode the menu before doing it properly *)

    let draw_texture name position =
      Render.draw_txr window name position 0.
    in

    let (w,h) = foi2D window#get_size in

    draw_texture "title" (w/.2., h /. 2. -. 250.);
    draw_texture "gameon_hover" (w/.2., h /. 2. +. 30.);
    draw_texture "gameon" (w /. 2., h /. 2. +. 30.);
    draw_texture "quit" (w /. 2. -. 130., h /. 2. +. 230.);
    draw_texture "settings" (w /. 2. +. 100., h /.2. +. 220.);

    window#display

  initializer

    if not (font#load_from_file "resources/fonts/Roboto-Regular.ttf")
    then failwith "Couldn't load the font here";
    if not (splash_font#load_from_file "resources/fonts/AdvoCut.ttf")
    then failwith "Couldn't load the font here";
    ignore(Interpolators.new_sine_ip
      self#set_alpha 2. 0.4 0.6);
    ignore(Interpolators.new_sine_ip
      self#set_splash_size 1.8 0.05 1.)

end
