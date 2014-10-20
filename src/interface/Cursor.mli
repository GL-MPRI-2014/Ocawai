class cursor : position:Position.t -> object

  method set_position : Position.t -> unit

  method position : Position.t

  method set_moving : unit -> unit

  method get_move : Action.movement

end
