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

  val mutable screen = new Home.screen [] []

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

    screen#draw window;

    window#display

  initializer

    if not (font#load_from_file "resources/fonts/Roboto-Regular.ttf")
    then failwith "Couldn't load the font here";
    if not (splash_font#load_from_file "resources/fonts/AdvoCut.ttf")
    then failwith "Couldn't load the font here";
    (* ignore(Interpolators.new_sine_ip
      self#set_alpha 2. 0.4 0.6);
    ignore(Interpolators.new_sine_ip
      self#set_splash_size 1.8 0.05 1.) *)
    (* TODO handle resizing with that too ! *)
    let window = manager#window in
    let (w,h) = foi2D window#get_size in
    screen <- new Home.screen
      [new Home.item "title" (w/.2., h /. 2. -. 250.)]
      [
        new Home.actionnable "gameon" "gameon_hover" (w/.2., h /. 2. +. 30.)
          (fun () -> Printf.printf "gameon!\n") ;
        new Home.actionnable "quit" "quit_hover"
          (w /. 2. -. 130., h /. 2. +. 230.)
          (fun () -> Printf.printf "quit\n") ;
        new Home.actionnable "settings" "settings_hover"
          (w /. 2. +. 100., h /.2. +. 220.)
          (fun () -> Printf.printf "settings\n")
      ]

end
