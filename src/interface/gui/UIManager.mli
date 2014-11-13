class ui_manager : object

  (** Returns true iff the event has been catched by one widget *)
  method on_event : OcsfmlWindow.Event.t -> bool

  (** Focus on a widget *)
  method focus : #Widget.widget -> unit

  (** Stops the focus on the given widget (does nothing if the widget was 
    * not focused) *)
  method unfocus : #Widget.widget -> unit

  (** Returns true iff a widget is focused *)
  method is_focusing : bool

  (** Adds a widget to the manager. Should only be used with 
    * widgets with no parent *)
  method add_widget : #Widget.widget -> unit

  (** Draws the UI *)
  method draw : OcsfmlGraphics.render_target -> TextureLibrary.t -> unit

end
