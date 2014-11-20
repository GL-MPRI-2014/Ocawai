open OcsfmlGraphics
open OcsfmlWindow
open GuiTools
open Widget
open BaseMixins
open Utils
open Menus

let my_font = new font (`File "resources/fonts/Roboto-Regular.ttf")

class text_framed_item m_position m_size m_text
  (action : unit -> unit) m_theme = object(self)

  inherit widget

  inherit themed_widget

  val mutable position = m_position

  val mutable size = m_size

  val mutable theme = m_theme

  method draw target lib = if self#active then begin
    let position = foi2D self#position in
    new rectangle_shape ~size:(foi2D size) ~position
      ~fill_color:(Color.rgba 0 0 0 0) ~outline_thickness:2.
      ~outline_color:theme.Theme.border_color ()
    |> target#draw;
    rect_print
      target m_text my_font Color.black (Pix (snd size - 3)) (Pix 2) Center {
        left = fst position ;
        top = snd position ;
        width = float_of_int (fst size) ;
        height = float_of_int (snd size) }
  end

  method action = action ()

end


class ingame_popup ~m_position ~m_size ~m_theme ~m_text ~m_bar_height
  ~m_bar_icon ~m_bar_text = object(self)

    inherit [text_framed_item] widget_container as super

    inherit key_ctrl_list KeyCode.Left KeyCode.Right

    inherit has_toolbar as toolbar

    val mutable nb_items = 0

    val mutable position = m_position

    val mutable size = m_size

    val mutable theme = m_theme

    val mutable toolbar_height = m_bar_height

    val mutable toolbar_text = m_bar_text

    val mutable toolbar_icon = m_bar_icon

    method add_child w =
      super#add_child w;
      nb_items <- nb_items + 1

    method draw target lib = if self#active then begin
      let active_widget = List.nth self#children self#selected in
      new rectangle_shape ~fill_color:theme.Theme.default_color
        ~size:(foi2D (fst size, snd size+m_bar_height-2))
        ~position:(foi2D (fst self#position, snd self#position-m_bar_height+2))
        ~outline_thickness:2. ~outline_color:theme.Theme.border_color ()
      |> target#draw;
      toolbar#draw target lib;
      new rectangle_shape ~fill_color:(theme.Theme.highlight_color)
        ~size:(foi2D active_widget#get_size)
        ~position:(foi2D active_widget#position) ()
      |> target#draw;
      rect_print
        target m_text my_font Color.black (Pix 18) (Pix 2) Center {
        left = float_of_int (fst position) ;
        top = float_of_int (snd position) ;
        width = float_of_int (fst size) ;
        height = float_of_int (snd size) };
      List.iter (fun w -> w#draw target lib) self#children
    end

  initializer
    self#add_event(function
      | Event.KeyPressed{ Event.code = KeyCode.Return ; _ }
      | Event.KeyPressed{ Event.code = KeyCode.Space ; _ } ->
          nb_items <> 0 &&
          ((List.nth self#children self#selected)#action;
          true)
      | _ -> false)

end
