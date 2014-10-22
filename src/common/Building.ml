type t = { name : string; product : Unit.t list;
           income : Resource.t; pos : Position.t }

let get_name t = t.name

let get_producible t = t.product

let get_income t = t.income

let get_position t = t.pos
