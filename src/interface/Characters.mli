(** Definitions concerning characters handled by the interface *)

(** Type of characters *)
type t =
  | Flatman
  | Blub
  | Limboy

(** Turns a [t] into a [string] *)
val to_string : t -> string

(** Object to handle characters of the interface *)
val handler : <

  (** @param player_id player id
    * @return the corresponding character *)
  character_from_id : int -> t ;

  (** @param player the player
    * @return the character of a player *)
  character_of : Player.logicPlayer -> t ;

  (** @param player_id the id of the player
    * @param name the unit/building name
    * @return the name of the texture associated to the character of the
    * player given as argument *)
  texture_from_id : int -> string -> string ;

  (** @param constraints the list of pairs (id,character)
    * @param players list of player ids
    * Initializes (or reset) the distribution of characters,
    * taking the constraints into account *)
  init : (int * t) list -> int list -> unit

>
