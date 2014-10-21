(* Building interface (draft) *)

type t = {name : string; product : Unit.t list; 
		  income : Resource.t; pos : Position t}

val get_name : t -> string

(* List of Units a Building is able to produce *)
val get_producible : t -> Unit.t list

(* Income of a given building *)
val get_income : t -> Resource.t

val get_position : t -> Position.t
