class ui_manager : object

  (** Returns true iff the event has been catched by one widget *)
  method on_event : OcsfmlWindow.Event.t -> bool

  (** Adds a widget to the manager. Should only be used with 
    * widgets with no parent *)
  method add_widget : Widget.widget -> unit

  (** Draws the UI *)
  method draw : OcsfmlGraphics.render_target -> TextureLibrary.t -> unit

end
