(** [new t width height nbplayers] genere une map de taille [width * height] et une liste d'armees, de taille [nbplayers] *)
class t : int -> int -> int -> object
  method field : Battlefield.t
  method armies : Unit.t list list
  method spawns : Position.t list
end

