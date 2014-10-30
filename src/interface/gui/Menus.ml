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
  ~callback ~m_theme = object(self)

  inherit widget

  inherit themed_widget

  val mutable position = m_position

  val mutable size = m_size

  val mutable theme = m_theme

  initializer
    self#add_event (function
      |Event.KeyPressed {Event.code = kc; _ } when keycode = kc ->
          (callback (); true)
      | _ -> false)

  method draw target lib = if self#active then begin
    new rectangle_shape ~fill_color:theme.Theme.default_color
      ~size:(foi2D size) ~position:(foi2D self#position) ()
    |> target#draw;

    let texture = TextureLibrary.(get_texture lib icon) in
    let (sx, sy) = foi2D texture#default_size in
    let (selfx, selfy) = foi2D size in
    let tex_size_x = sx *. selfy /. sy in
    let position = foi2D self#position in
    texture#draw ~target ~position ~size:(tex_size_x, selfy) ();

    rect_print
      target text my_font Color.black (Pix (snd size - 1)) (Pix 2) Center {
        left = fst position +. tex_size_x ;
        top = snd position ;
        width = selfx -. tex_size_x ;
        height = selfy }
  end

end


class key_button_oneuse ~icon ~text ~m_position ~m_size ~keycode
  ~callback ~m_theme = object(self)

  inherit key_button ~icon ~text ~m_position ~m_size ~keycode ~callback 
    ~m_theme

  initializer
    self#add_event(function
      |Event.KeyPressed {Event.code = kc; _ } when keycode = kc ->
          (self#toggle; true)
      | _ -> false)
end

class menu pos width i_height keycode m_theme = object(self)

  inherit [item] evq_container as super

  inherit key_ctrl_list

  val mutable position = pos

  val mutable size = (width, 0)

  val mutable nb_items = 0

  val mutable item_height = i_height

  val mutable theme = m_theme

  method draw target lib = if self#active then begin
    new rectangle_shape ~fill_color:theme.Theme.default_color
      ~size:(foi2D size) ~position:(foi2D self#position) ()
    |> target#draw;
    let (posx, posy) = self#position in
    new rectangle_shape ~fill_color:(theme.Theme.highlight_color)
      ~size:(foi2D (width, item_height)) 
      ~position:(foi2D (posx, posy + self#selected * item_height)) ()
    |> target#draw;
    List.iter (fun w -> w#draw target lib) self#children end 

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
