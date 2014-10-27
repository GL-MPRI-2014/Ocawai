(** The state that represents the game itself *)

(** Game Screen state *)
class game : object

  inherit State.state

  method create_ui : UIManager.ui_manager -> Menus.menu

end
