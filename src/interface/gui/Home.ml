open Utils

class item name position = object(self)

  method draw target =
    Render.draw_txr target name position 0.

end

class actionnable txr txr_hover position (action : unit -> unit) = object(self)

  val mutable selected = false

  inherit item txr position as super

  method action = action ()

  method set_selected b = selected <- b

  method draw target =
    if selected then Render.draw_txr target txr_hover position 0. ;
    super#draw target

end

class screen items actionnables = object(self)

  val mutable selected = None

  method private select actionnable =
    selected >? (fun o -> o#set_selected false) ;
    selected <- Some actionnable ;
    actionnable#set_selected true

  method draw (target : OcsfmlGraphics.render_window) =

    List.iter (fun i -> i#draw target) items ;
    List.iter (fun a -> a#draw target) actionnables

  initializer
    match actionnables with
    | a :: _ -> self#select a
    | _ -> ()

end
