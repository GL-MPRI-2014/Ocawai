open OcsfmlGraphics
open OcsfmlWindow
open Widget
open BaseMixins
open Utils

let my_font = new font (`File "resources/fonts/AdvoCut.ttf")

class item icon text (action : unit -> unit) = object(self)

  inherit widget

  (* Undefined until added to a container *)
  val mutable position = (0,0)

  val mutable size = (0,0)

  method draw target lib = 
    (* First draw the background and the icon *)
    let texture = TextureLibrary.(get_texture lib icon) in
    let (sx, sy) = foi2D texture#get_size in
    let (selfx, selfy) = foi2D size in
    let scale = selfy /. sy in
    let position = foi2D self#position in
    new rectangle_shape ~outline_thickness:1. ~fill_color:Color.white 
      ~outline_color:Color.black ~size:(selfx, selfy) ~position ()
    |> target#draw;
    new sprite ~texture ~scale:(scale,scale) ~position ()
    |> target#draw;
    (* Then draw the text *)
    new text ~string:text ~character_size:(snd size - 1)
      ~position:(fst position +. sx *. scale +. 5., snd position -. 2.)
      ~font:my_font ~color:Color.black ()
    |> target#draw

  method action = action ()

end


class menu pos width i_height = object(self)

  inherit [item] evq_container as super

  inherit key_ctrl_list

  val mutable position = pos
  
  val mutable size = (width, 0)

  val mutable nb_items = 0

  val mutable item_height = i_height

  method draw target lib = List.iter (fun c -> c#draw target lib) 
    self#children;
    let (posx, posy) = self#position in
    new rectangle_shape ~outline_thickness:2. ~fill_color:(Color.rgba 0 0 0 0)
      ~outline_color:Color.blue ~size:(foi2D (width,item_height))
      ~position:(foi2D (posx, posy + self#selected * item_height)) ()
    |> target#draw

  method add_child w = 
    super#add_child w;
    nb_items <- nb_items + 1
end
