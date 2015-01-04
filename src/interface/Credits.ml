open OcsfmlGraphics
open Utils
open GuiTools

open Manager

class state = object(self)

  inherit State.state as super

  method private set_screen w h =
    let (w,h) = foi2D (w,h) in ()

  method handle_event e =
    OcsfmlWindow.Event.(
      match e with
      | Resized { width = w ; height = h } -> self#set_screen w h
      | KeyPressed { code = OcsfmlWindow.KeyCode.Escape ; _ }
      | KeyPressed { code = OcsfmlWindow.KeyCode.Q ; _ } ->
          manager#pop
      | _ -> ()
    )

  method render window =

    (* Interpolators.update (); *)

    let color = Color.rgb 221 224 234 in
    window#clear ~color ();

    (* screen#draw window; *)

    window#display

  initializer
    let window = manager#window in
    let (w,h) = window#get_size in
    self#set_screen w h

end
