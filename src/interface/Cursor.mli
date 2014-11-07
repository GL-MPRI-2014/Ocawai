
(** This is the status of the cursor *)
type cursor_state = Idle 
  | Displace of Battlefield.t * Unit.t * Logics.accessibles
  | Action of Unit.t * Position.t

(** This class represents the cursor on the map *)
class cursor : position:Position.t -> object

  (** Sets the position of the cursor *)
  method set_position : Position.t -> unit

  (** @return the position of the cursor *)
  method position : Position.t

  (** Sets the cursor state *)
  method set_state : cursor_state -> unit

  (** Get the cursor state *)
  method get_state : cursor_state

  (** Get the current movement (useful only if movement is beeing recorded) *)
  method get_move : Action.movement

  (** Interpolation (do not interfere) *)
  method scale : float

  (** Interpolation *)
  method offset : float * float

  (** Interpolation *)
  method set_offset : float * float -> unit

end
