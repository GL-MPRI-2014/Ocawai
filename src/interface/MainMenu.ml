open OcsfmlGraphics
open Utils

open Manager

class main_menu = object(self)

  inherit State.state as super

  val font = new font `None

  method handle_event e =

    OcsfmlWindow.Event.(
      match e with
        | KeyPressed { code = OcsfmlWindow.KeyCode.Space ; _ } ->
            (new Game.game :> State.state) |> manager#push
        | _ -> ()
    )

  method render window =

    super#render window ;
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
    text#set_position ((w -. text_width) /. 2.) (200.);
    window#draw text ;

    let text : text = new text
      ~string:"Press space to begin."
      ~font
      ~character_size:60
      ~color:Color.white
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
    then failwith "Couldn't load the font here"

end
