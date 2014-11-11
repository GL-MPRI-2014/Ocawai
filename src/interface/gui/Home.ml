open Utils

class virtual future_item = object(self)

  method virtual draw : OcsfmlGraphics.render_window -> unit
  method virtual position : float * float

  method x = fst self#position
  method y = snd self#position

end

class virtual future_actionnable = object(self)

  inherit future_item as super

  val mutable has_focus = false

  method set_focus b = has_focus <- b

  method virtual action : unit

end

class item name position = object(self)

  method draw target =
    Render.draw_txr target name position 0.

  method position = position
  method x = fst position
  method y = snd position

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

  method handle_key = OcsfmlWindow.KeyCode.(function
    | Left -> self#left
    | Right -> self#right
    | Up -> self#up
    | Down -> self#down
    | Return -> self#action
    | _ -> ()
  )

  method private action =
    selected >? fun o -> o#action

  method private sqdist a b =
    let x = a#x -. b#x
    and y = a#y -. b#y in
    int_of_float (x *. x +. y *. y)

  method private weight horizontal s a =
    if horizontal then
      self#sqdist a s * (abs (int_of_float (a#y -. s#y)))
    else
      self#sqdist a s * (abs (int_of_float (a#x -. s#x)))

  method private compare h s a b =
    (self#weight h s a - self#weight h s b)

  method private move p h = selected >? fun s ->
    List.find_all (fun a -> a <> s && p a s) actionnables
    |> List.sort (self#compare h s)
    |> function
        | a :: _ -> self#select a
        | _ -> ()

  method private left = self#move (fun a s -> a#x <= s#x) true
  method private right = self#move (fun a s -> a#x >= s#x) true
  method private up = self#move (fun a s -> a#y <= s#y) false
  method private down = self#move (fun a s -> a#y >= s#y) false

  initializer
    match actionnables with
    | a :: _ -> self#select a
    | _ -> ()

end
