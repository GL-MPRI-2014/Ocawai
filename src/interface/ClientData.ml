class client_data ~(map:Battlefield.t) ~(camera:Camera.camera) 
  ~(units:Unit.t list) = object

  val mutable selected_unit : Unit.t option = None

  val mutable current_move : Position.t list = []

  method map = map

  method camera = camera

  (* Will be useful later *)
  (* method players : Player.t list *)

  (* Will be deleted later *)
  method units = units

  method select_unit u = selected_unit <- Some u

  method selected = selected_unit

  (* Maybe a Displacement module would be useful here *)
  method set_current_move l = current_move <- l

  method current_move = current_move 

end
