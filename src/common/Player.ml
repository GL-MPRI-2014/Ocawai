let get_army t = t.army

let add_unit t u = t.army <- (u::(t.army))

let get_buildings t = t.buidings

let add_building t b = t.buildings <- (b::(t.buildings))

let get_next_action t  = ((0,0),Wait)