open OcsfmlGraphics
open OcsfmlWindow
open Utils
open GuiTools
open Widget

(* Put it in the theme ? *)
let bold_font = new font (`File "resources/fonts/Roboto-Black.ttf")


class virtual ['a] widget_container = object(self)

  inherit widget as super

  constraint 'a = #widget

  val mutable children : 'a list = []

  method add_child (w : 'a) =
    children <- w::children;
    w#set_parent (Some (self :> widget));
    self#add_event (fun e -> w#on_event e)

  method children = children

  method toggle =
    super#toggle;
    List.iter (fun c -> c#toggle) children


end


class virtual themed_widget = object(self)

  val virtual mutable theme : Theme.t

end


class virtual ['a] evq_container = object(self)

  inherit ['a] widget_container as super

  val virtual mutable item_height : int

  method add_child w =
    super#add_child w;
    size <- (fst size, snd size + item_height);
    w#set_size (fst size, item_height);
    w#set_position (0, (List.length children - 1) * item_height)


end



class virtual key_ctrl_list key1 key2 = object(self)

  val mutable selected = 0

  val virtual mutable nb_items : int

  method virtual add_event : (Event.t -> bool) -> unit

  method selected = selected

  initializer
    self#add_event (function
      |Event.KeyPressed {Event.code = kc; _} when kc = key1 ->
          nb_items <> 0
          && (selected <- (selected - 1 + nb_items) mod nb_items; true)
      |Event.KeyPressed {Event.code = kc; _} when kc = key2 ->
          nb_items <> 0
          && (selected <- (selected + 1 + nb_items) mod nb_items; true)
      | _ -> false)
end


class virtual has_toolbar = object(self)

  inherit Widget.widget

  inherit themed_widget

  val virtual mutable toolbar_height : int

  val virtual mutable toolbar_icon : string

  val virtual mutable toolbar_text : string

  method draw target lib =
    let position = foi2D (sub2D self#position (0, toolbar_height)) in
    new rectangle_shape ~position ~size:(foi2D (fst size, toolbar_height))
      ~fill_color:theme.Theme.bar_color ()
    |> target#draw;
    let texture = TextureLibrary.get_texture lib toolbar_icon in
    let (sx, sy) = texture#default_size in
    let tex_size_x = sx * toolbar_height / sy in
    texture#draw ~target ~position ~size:(foi2D (tex_size_x, toolbar_height))
    () ;
    let (posx,posy) = position in
    let (sx,sy) = foi2D (fst size, toolbar_height) in
    let tx = float_of_int tex_size_x in
    rect_print
      target toolbar_text bold_font Color.white (Pix (toolbar_height - 3))
        (Pix 2) Left {
          left = posx +. tx ;
          top = posy ;
          width = sx -. tx ;
          height = sy }

end
