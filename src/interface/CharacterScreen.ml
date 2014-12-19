open OcsfmlGraphics
open Utils
open GuiTools

open Manager

class state = object(self)

  inherit State.state as super

  val mutable screen = new Home.screen [] []

  val font = Fonts.load_font "FreeSansBold.ttf"

  method private set_screen w h =
    let (w,h) = foi2D (w,h) in
    screen <- new Home.screen
      []
      [
        (* Choosing flatman *)
        new Home.textured_actionnable "flatman" "flatman_hover"
          (w/.2., h /. 2. +. 30.)
          (fun () -> manager#push
            (new LoadScreen.state (Game.new_game 0) :> State.state)) ;
        (* Choosing Blub as main character *)
        new Home.textured_actionnable "blub" "blub_hover"
          (w/.2. -. 220., h /. 2. +. 30.)
          (fun () -> manager#push
            (new LoadScreen.state (Game.new_game 1) :> State.state)) ;
        (* Or limboy *)
        new Home.textured_actionnable "limboy" "limboy_hover"
          (w/.2. +. 220., h /. 2. +. 30.)
          (fun () -> manager#push
            (new LoadScreen.state (Game.new_game 2) :> State.state)) ;
        (* Back button *)
        new Home.textured_actionnable "back" "back_hover" (200., h -. 100.)
          (fun () -> manager#pop)
      ]

  method handle_event e =

    OcsfmlWindow.Event.(
      match e with
        | Resized { width = w ; height = h } -> self#set_screen w h
        | KeyPressed { code = kc ; _ } ->
            screen#handle_key kc
        | _ -> ()
    )

  method render window =

    let color = Color.rgb 221 224 234 in
    window#clear ~color ();

    let (w,h) = foi2D window#get_size in

    rect_print
      window "CHOOSE YOUR CHARACTER" font Color.black (Pix 70) (Pix 10) Center
      { left = 10. ; top = 30. ; width = w -. 20. ; height = 100. };

    rect_print
      window "And choose wisely because it only changes the textures."
      font (Color.rgba 0 0 0 100) (Pix 27) (Pix 10) Center
      { left = 10. ; top = 95. ; width = w -. 20. ; height = 50. };

    screen#draw window;

    window#display

  initializer
    let window = manager#window in
    let (w,h) = window#get_size in
    self#set_screen w h

end
