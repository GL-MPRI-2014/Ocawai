open OcsfmlGraphics
open Utils
open GuiTools

open Manager

class main_menu = object(self)

  inherit State.state as super

  val mutable screen = new Home.screen [] []

  val key_seq = [
    OcsfmlWindow.KeyCode.Up;
    OcsfmlWindow.KeyCode.Up;
    OcsfmlWindow.KeyCode.Down;
    OcsfmlWindow.KeyCode.Down;
    OcsfmlWindow.KeyCode.Left;
    OcsfmlWindow.KeyCode.Right;
    OcsfmlWindow.KeyCode.Left;
    OcsfmlWindow.KeyCode.Right;
    OcsfmlWindow.KeyCode.B;
    OcsfmlWindow.KeyCode.A
  ]

  val mutable remaining_keys = [
    OcsfmlWindow.KeyCode.Up;
    OcsfmlWindow.KeyCode.Up;
    OcsfmlWindow.KeyCode.Down;
    OcsfmlWindow.KeyCode.Down;
    OcsfmlWindow.KeyCode.Left;
    OcsfmlWindow.KeyCode.Right;
    OcsfmlWindow.KeyCode.Left;
    OcsfmlWindow.KeyCode.Right;
    OcsfmlWindow.KeyCode.B;
    OcsfmlWindow.KeyCode.A
  ]

  method private handle_keys e =
    match remaining_keys with
    | key :: r -> begin
        OcsfmlWindow.Event.(match e with
        | KeyPressed { code = k ; _ } when k = key -> remaining_keys <- r
        | KeyPressed _ -> remaining_keys <- key_seq
        | _ -> ())
      end
    | [] ->
        (new Snake.state :> State.state) |> manager#push ;
        remaining_keys <-key_seq


  method private set_screen w h =
    let (w,h) = foi2D (w,h) in
    screen <- new Home.screen
      [new Home.item "title" (w/.2., h /. 2. -. 250.)]
      [
        new Home.actionnable "gameon" "gameon_hover" (w/.2., h /. 2. +. 30.)
          (fun () -> (new Game.game :> State.state) |> manager#push) ;
        new Home.actionnable "quit" "quit_hover"
          (w /. 2. -. 130., h /. 2. +. 230.)
          (fun () -> manager#window#close) ;
        new Home.actionnable "settings" "settings_hover"
          (w /. 2. +. 100., h /.2. +. 220.)
          (fun () -> Printf.printf "settings\n")
      ]

  method handle_event e =

    self#handle_keys e;

    OcsfmlWindow.Event.(
      match e with
        | Resized { width = w ; height = h } -> self#set_screen w h
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

    Interpolators.update ();

    let color = Color.rgb 221 224 234 in
    window#clear ~color ();

    screen#draw window;

    window#display

  initializer
    let window = manager#window in
    let (w,h) = window#get_size in
    self#set_screen w h

end
