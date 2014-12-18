open OcsfmlGraphics
open OcsfmlWindow
open GuiTools
open Widget
open BaseMixins
open Utils

let my_font = Fonts.load_font "FreeSans.ttf"

class item ?enabled:(enabled = true) icon text (action : unit -> unit) =
  object(self)

  inherit widget

  (* Undefined until added to a container *)
  val mutable position = (0,0)

  val mutable size = (0,0)

  method draw target lib = if self#active then begin
    (* First draw the icon *)
    let color = if enabled
      then Color.rgb 255 255 255
      else Color.rgb 150 150 150
    in
    let position = foi2D self#position in
    let (selfx, selfy) = foi2D size in
    Render.renderer#draw_txr target icon ~position:position
      ~size:(selfy, selfy) ~centered:false ~color ();
    (* Then draw the text *)
    rect_print
      target text my_font
        (if enabled then Color.black else Color.rgba 0 0 0 127)
        (Pix (snd size - 3)) (Pix 2) Left {
        left = fst position +. selfy ;
        top = snd position ;
        width = selfx -. selfy ;
        height = selfy }
  end

  method action = if enabled then action ()

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
      ~size:(foi2D size) ~position:(foi2D self#position)
      ~outline_color:theme.Theme.border_color
      ~outline_thickness:2. ()
    |> target#draw;

    let (selfx, selfy) = foi2D size in
    let position = foi2D self#position in
    Render.renderer#draw_txr target icon ~position ~size:(selfy, selfy)
      ~centered:false ();

    rect_print
      target text my_font Color.black (Pix (snd size - 1)) (Pix 2) Center {
        left = fst position +. selfy ;
        top = snd position ;
        width = selfx -. selfy ;
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

class ingame_menu ~m_position ~m_width ~m_item_height ~m_theme ~m_bar_height
  ~m_bar_icon ~m_bar_text
  = object(self)

  inherit [item] evq_container as super

  inherit key_ctrl_list OcsfmlWindow.KeyCode.Up OcsfmlWindow.KeyCode.Down as kcl

  inherit has_toolbar as toolbar

  val mutable position = m_position

  val mutable size = (m_width, 0)

  val mutable nb_items = 0

  val mutable item_height = m_item_height

  val mutable theme = m_theme

  val mutable toolbar_height = m_bar_height

  val mutable toolbar_icon = m_bar_icon

  val mutable toolbar_text = m_bar_text

  method draw target lib = if self#active then begin
    new rectangle_shape ~fill_color:theme.Theme.default_color
      ~size:(foi2D (fst size, snd size+m_bar_height-2))
      ~position:(foi2D (fst self#position, snd self#position-m_bar_height+2))
      ~outline_thickness:2. ~outline_color:theme.Theme.border_color ()
    |> target#draw;
    toolbar#draw target lib;
    let (posx, posy) = self#position in
    new rectangle_shape ~fill_color:(theme.Theme.highlight_color)
      ~size:(foi2D (m_width, item_height))
      ~position:(foi2D (posx, posy + self#selected * item_height)) ()
    |> target#draw;
    List.iter (fun w -> w#draw target lib) self#children
  end

  method add_child w =
    super#add_child w;
    nb_items <- nb_items + 1

  method clear_children =
    nb_items <- 0 ;
    super#clear_children

  method toggle =
    super#toggle ;
    toolbar#toggle ;
    kcl#reset_selection


  initializer
    self#add_event(function
      | Event.KeyPressed {Event.code = KeyCode.Return ; _ }
      | Event.KeyPressed { Event.code = KeyCode.Space ; _ } ->
          nb_items <> 0 &&
          ((List.nth self#children (nb_items - self#selected - 1))#action;
          true)
      | _ -> false)

end
