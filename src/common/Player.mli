type t

val get_army : t -> Unit.t list

val add_unit : t -> Unit.t -> unit

val get_buildings : t -> Building.t list

val add_building : t -> Building.t -> unit

val get_next_action : t -> (Action.movement * Action.action)
