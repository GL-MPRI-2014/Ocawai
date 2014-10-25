(** [new t width height nbplayers] genere une map et une liste d'armees, de taille nbplayers *)
class t : int -> int -> int -> object
  method field : Battlefield.t
  method armies : Unit.t list list
end

