open OcsfmlGraphics
open Utils
open GuiTools

open Manager

class main_menu = object(self)

  inherit State.state as super

  val mutable screen = new Home.screen [] []

  val bg_texture = new texture (`File ((Utils.base_path ()) ^ "textures/gui/capture.png"))
  val mutable bg_offset = (0.,0.)
  val mutable bg_dir = (0.,0.)
  val mutable music_run = ref true

  val key_seq = OcsfmlWindow.KeyCode.([
    Up;
    Up;
    Down;
    Down;
    Left;
    Right;
    Left;
    Right;
    B;
    A
  ])

  val mutable remaining_keys = OcsfmlWindow.KeyCode.([
    Up;
    Up;
    Down;
    Down;
    Left;
    Right;
    Left;
    Right;
    B;
    A
  ])

  method private update_offset =
    bg_offset <- Utils.addf2D bg_offset bg_dir;
    let ox, oy = bg_offset in
    if ox <= 0. && oy <= 0. then
      bg_dir <- (0.22, 0.)
    else if ox >= 200. && oy <= 0. then
      bg_dir <- (0., 0.22)
    else if ox >= 200. && oy >= 200. then
      bg_dir <- (-0.22, 0.)
    else if ox <= 0. && oy >= 200. then
      bg_dir <- (0., -0.22)


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
      [new Home.textured_item "title" (w/.2., h /. 2. -. 250.)]
      [
        new Home.textured_actionnable "gameon" "gameon_hover"
          (w/.2., h /. 2. +. 30.)
          (fun () -> new CharacterScreen.state |> manager#push) ;
        new Home.textured_actionnable "quit" "quit_hover"
          (w /. 2. -. 130., h /. 2. +. 230.)
          (fun () -> manager#window#close) ;
        new Home.textured_actionnable "settings" "settings_hover"
          (w /. 2. +. 100., h /.2. +. 220.)
          (fun () -> new SettingsScreen.state |> manager#push) ;
        new Home.textured_actionnable "credits" "credits_hover"
          (w /. 2. -. 50., h -. 50.)
          (fun () -> new Credits.state |> manager#push)
      ]

  method handle_event e =

    self#handle_keys e;

    OcsfmlWindow.Event.(
      match e with
        | Resized { width = w ; height = h } -> self#set_screen w h
        | KeyPressed { code = kc ; _ } ->
            screen#handle_key kc
        | _ -> ()
    )

  method render window =

    Interpolators.update ();

    self#update_offset;

    let color = Color.rgb 221 224 234 in
    window#clear ~color ();

    let (w,h) = Utils.foi2D window#get_size in
    let (tw,th) = Utils.foi2D bg_texture#get_size in
    new sprite ~texture:bg_texture ~scale:(w *. 1.5 /. tw, h *. 1.5 /. th)
      ~position:(subf2D (0.,0.) bg_offset) ()
    |> window#draw;

    screen#draw window;

    window#display

  method paused =
    music_run := false

  method resumed =
    music_run := true

  initializer
    let window = manager#window in
    let (w,h) = window#get_size in
    self#set_screen w h;
    let music_player = MusicPlayer.music_player () in
    ignore @@ Thread.create (music_player#play_menu) (music_run)

end
