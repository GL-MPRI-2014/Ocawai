open OcsfmlGraphics
open Utils

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
        | KeyPressed { code = OcsfmlWindow.KeyCode.Space ; _ } ->
            (new Game.game :> State.state) |> manager#push
        | _ -> ()
    )

  method render window =

    super#render window ;

    Interpolators.update ();

    window#clear ();

    let text : text = new text
      ~string:"PGL"
      ~font
      ~character_size:200
      ~color:Color.white
      ()
    in
    let (w,h) = window#get_size in
    let (w,h) = float_of_int w, float_of_int h in
    let text_width = text#get_global_bounds.width in
    text#set_position ((w -. text_width) /. 2.) (150.);
    window#draw text ;

    let text : text = new text
      ~string:"Now with rivers !"
      ~font:splash_font
      ~character_size:30
      ~color:Color.yellow
      ~scale:(splash_size, splash_size)
      ()
    in
    let tbounds = text#get_global_bounds in
    text#set_origin (tbounds.width /. 2.) (tbounds.height /. 2.);
    text#set_position ((w +. text_width) /. 2. -. 80.) 330.;
    text#set_rotation (-20.);
    window#draw text;

    let text : text = new text
      ~string:"Press space to begin."
      ~font
      ~character_size:60
      ~color:(Color.rgba 255 255 255 (int_of_float (255. *. text_alpha)))
      ()
    in
    let (w,h) = window#get_size in
    let (w,h) = float_of_int w, float_of_int h in
    let text_width = text#get_global_bounds.width in
    text#set_position ((w -. text_width) /. 2.) (h -. 200.);
    window#draw text ;

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
