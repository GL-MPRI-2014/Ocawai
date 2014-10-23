type t = { mutable army : Unit.t list; mutable buildings : Building.t list }

let get_army t = t.army

let add_unit t u = t.army <- (u::(t.army))

let get_buildings t = t.buildings

let add_building t b = t.buildings <- (b::(t.buildings))

let get_next_action t  = [],Action.Wait
