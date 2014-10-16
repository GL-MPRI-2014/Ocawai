type t

val get_army : t -> Unit.t list

val get_buildings : t -> Building.t list

val get_next_action : t -> (Action.movement * Action.action)
