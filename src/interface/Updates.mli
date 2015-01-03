(** Module to handle updates in the interface *)

(** Class that represents update handlers *)
class handler : ClientData.client_data -> Camera.camera -> object

  (** Method to be called at each frame to update *)
  method update : unit

  (** @return the position and offset required to print a given unit *)
  method unit_position : Unit.t -> (Position.t * (float * float))

end
