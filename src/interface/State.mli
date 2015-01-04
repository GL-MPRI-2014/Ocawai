(** Contains the state class that represents the different screens *)

(** Abstract class for states *)
class virtual state : object

  (** Method ran every frame by the state manager *)
  method virtual render : OcsfmlGraphics.render_window -> unit

  (** Method called on very event transmitted by the manager *)
  method handle_event : OcsfmlWindow.Event.t -> unit

  (** Run when no longer on top of the stack *)
  method paused : unit

  (** Run when back to stack *)
  method resumed : unit

  (** Method run when state is ended *)
  method destroy : unit

end
