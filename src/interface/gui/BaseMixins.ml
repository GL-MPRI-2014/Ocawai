open OcsfmlWindow
open OcsfmlGraphics
open Utils

let my_font = new font (`File "resources/fonts/AdvoCut.ttf")

class virtual widget = object(self)

  val virtual mutable position : (int * int)

  val virtual mutable parent : widget option 

  val virtual mutable size : (int * int)

  val mutable event_funs : (Event.t -> unit) list = []

  method position = 
    match parent with
    |None -> position
    |Some(s) -> add2D s#position position

  method add_event f = event_funs <- f :: event_funs

  method on_event e = List.iter (fun f -> f e) event_funs

  method virtual draw : render_target -> TextureLibrary.t -> unit

end


class virtual transformable = object(self)

  val virtual mutable position : int * int

  val virtual mutable size : int * int

  method set_size s = size <- s

  method set_position p = position <- p

end


class virtual item icon text (action : unit -> unit) = object(self)

  inherit widget

  inherit transformable

  method draw target lib = 
    (* First draw the background and the icon *)
    let texture = TextureLibrary.(get_texture lib icon) in
    let (sx, sy) = foi2D texture#get_size in
    let (selfx, selfy) = foi2D size in
    let scale = (selfx /. sx, selfy /. sy) in
    let position = foi2D self#position in
    new rectangle_shape ~outline_thickness:2. ~fill_color:Color.white 
      ~outline_color:Color.black ~size:(selfx, selfy) ~position ()
    |> target#draw;
    new sprite ~texture ~scale ~position ()
    |> target#draw;
    (* Then draw the text *)
    new text ~string:text ~character_size:(snd size)
      ~font:my_font ~color:Color.black ()
    |> target#draw

  method action = action ()

end



