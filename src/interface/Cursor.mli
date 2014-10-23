(** This class represents the cursor on the map *)

class cursor : position:Position.t -> object

  (** Sets the position of the cursor *)
  method set_position : Position.t -> unit

  (** @return the position of the cursor *)
  method position : Position.t

  method set_moving : unit

  method stop_moving : unit

  method toggle_moving :  unit

  method get_move : Action.movement

  method scale : float

end
