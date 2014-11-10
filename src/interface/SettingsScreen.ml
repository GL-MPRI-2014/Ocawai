open OcsfmlGraphics
open Utils
open GuiTools

open Manager

class state = object(self)

  inherit State.state as super

  val mutable screen = new Home.screen [] []

  val font = new font `None

  method private set_screen w h =
    let (w,h) = foi2D (w,h) in
    screen <- new Home.screen
      []
      [
        new Home.actionnable "back" "back_hover" (w/.2., h /. 2. +. 30.)
          (fun () -> manager#pop) ;
      ]

  method handle_event e =

    OcsfmlWindow.Event.(
      match e with
        | KeyPressed { code = OcsfmlWindow.KeyCode.Back ; _ } ->
            manager#pop
        | KeyPressed { code = OcsfmlWindow.KeyCode.Left ; _ } ->
            screen#left
        | KeyPressed { code = OcsfmlWindow.KeyCode.Right ; _ } ->
            screen#right
        | KeyPressed { code = OcsfmlWindow.KeyCode.Up ; _ } ->
            screen#up
        | KeyPressed { code = OcsfmlWindow.KeyCode.Down ; _ } ->
            screen#down
        | KeyPressed { code = OcsfmlWindow.KeyCode.Return ; _ } ->
            screen#action
        | _ -> ()
    )

  method render window =

    super#render window ;

    let color = Color.rgb 221 224 234 in
    window#clear ~color ();

    let (w,h) = foi2D window#get_size in

    rect_print
      window "SETTINGS" font Color.black (Pix 70) (Pix 10) Left
      { left = 10. ; top = 10. ; width = w -. 20. ; height = 100. };

    screen#draw window;

    window#display

  initializer
    if not (font#load_from_file "resources/fonts/Roboto-Black.ttf")
    then failwith "Couldn't load the font here";
    let window = manager#window in
    let (w,h) = window#get_size in
    self#set_screen w h

  method destroy =
    ()

end
