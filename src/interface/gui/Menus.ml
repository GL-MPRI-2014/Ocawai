open OcsfmlGraphics
open OcsfmlWindow
open GuiTools
open Widget
open BaseMixins
open Utils

let my_font = new font (`File "resources/fonts/Roboto-Regular.ttf")

class item icon text (action : unit -> unit) = object(self)

  inherit widget

  (* Undefined until added to a container *)
  val mutable position = (0,0)

  val mutable size = (0,0)

  method draw target lib = if self#active then begin
    (* First draw the icon *)
    let texture = TextureLibrary.(get_texture lib icon) in
    let (sx, sy) = foi2D texture#default_size in
    let (selfx, selfy) = foi2D size in
    let tex_size_x = sx *. selfy /. sy in
    let position = foi2D self#position in
    texture#draw ~target ~position ~size:(tex_size_x, selfy) ();
    (* Then draw the text *)
    rect_print
      target text my_font Color.black (Pix (snd size - 1)) (Pix 2) Center {
        left = fst position +. tex_size_x ;
        top = snd position ;
        width = selfx -. tex_size_x ;
        height = selfy }
  end

  method action = action ()

end


class key_button ~icon ~text ~m_position ~m_size ~keycode
  ~callback = object(self)

  inherit widget

  val mutable position = m_position

  val mutable size = m_size

  initializer
    self#add_event (function
      |Event.KeyPressed {Event.code = kc; _ } when keycode = kc ->
          (callback (); true)
      | _ -> false)

  method draw target lib = if self#active then begin
    let texture = TextureLibrary.get_texture lib "menu" in
    texture#draw ~target ~position:(foi2D self#position) ~size:(foi2D size) ();

    let texture = TextureLibrary.(get_texture lib icon) in
    let (sx, sy) = foi2D texture#default_size in
    let (selfx, selfy) = foi2D size in
    let tex_size_x = sx *. selfy /. sy in
    let position = foi2D self#position in
    texture#draw ~target ~position ~size:(tex_size_x, selfy) ();

    let txt = new text ~string:text ~character_size:(snd size - 1)
      ~font:my_font ~color:Color.black () in
    let txt_bounds = txt#get_global_bounds in
    txt#set_origin (txt_bounds.width /. 2.) (txt_bounds.height /. 2.);
    txt#set_position (fst position +. (selfx +. tex_size_x)/.2.)
                     (snd position +. selfy /. 4.);
    target#draw txt
  end

end


class key_button_oneuse ~icon ~text ~m_position ~m_size ~keycode
  ~callback = object(self)

  inherit key_button ~icon ~text ~m_position ~m_size ~keycode ~callback

  initializer
    self#add_event(function
      |Event.KeyPressed {Event.code = kc; _ } when keycode = kc ->
          (self#toggle; true)
      | _ -> false)
end

class menu pos width i_height keycode = object(self)

  inherit [item] evq_container as super

  inherit key_ctrl_list

  val mutable position = pos

  val mutable size = (width, 0)

  val mutable nb_items = 0

  val mutable item_height = i_height

  method draw target lib = if self#active then begin
    let (selfx, selfy) = foi2D size in
    let position = foi2D self#position in
    (*new rectangle_shape ~outline_thickness:1. ~fill_color:Color.white
      ~outline_color:Color.black ~size:(selfx, selfy) ~position ()
    |> target#draw;*)
    let texture = TextureLibrary.get_texture lib "menu" in
    texture#draw ~target ~position ~size:(selfx, selfy) ();
    List.iter (fun c -> c#draw target lib) self#children;
    let (posx, posy) = self#position in
    new rectangle_shape ~outline_thickness:2. ~fill_color:(Color.rgba 0 0 0 0)
      ~outline_color:Color.blue ~size:(foi2D (width,item_height))
      ~position:(foi2D (posx, posy + self#selected * item_height)) ()
    |> target#draw end

  method add_child w =
    super#add_child w;
    nb_items <- nb_items + 1

  initializer
    self#add_event(function
      |Event.KeyPressed{Event.code = kc; _} when keycode = kc ->
          nb_items <> 0 &&
          ((List.nth self#children (nb_items - self#selected - 1))#action;
          true)
      | _ -> false)

end
