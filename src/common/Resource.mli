(** Ressource interface (draft)
  * Holds the player's ressources (money, wood, what's not) *)

type t

(** Group operator *)
val add_ressource : t -> t -> t

val create : int -> t

val value : t -> int
