(** This class represents the cursor on the map *)

class cursor : position:Position.t -> object

  (** Sets the position of the cursor *)
  method set_position : Position.t -> unit

  (** @return the position of the cursor *)
  method position : Position.t

  (** Initiate movement recording *)
  method set_moving : unit

  (** Stop movement recording *)
  method stop_moving : unit

  (** Same as [set_moving] or [stop_moving] depending on what is active *)
  method toggle_moving :  unit

  (** Get the current movement (useful only if movement is beeing recorded) *)
  method get_move : Action.movement

  (** Interpolation (do not interfere) *)
  method scale : float

end
