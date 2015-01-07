(** Handles the Fog of War
  *
  * All functions check if the fog has been initialized *)

type t

(** @param w the width of the map
  * @param h the height of the map
  * @return an empty fog (nothing is visible) of size [w * h] *)
val init : int -> int -> t

(** @param fog the fog to copy
  * @return a copy of [fog] *)
val copy : t -> t

(** @param fog the fog of a certain player
  * @param pos the position you want to survey
  * @return [true] iff the case if under the fog of war *)
val hidden : t -> Position.t -> bool

(** @param fog the fog of a certain player
  * @param pos the position you want to survey
  * @param [true] iff the case if visible *)
val visible : t -> Position.t -> bool

(** Tells if a unit is hidden by the fog of war *)
val hidden_unit : t -> Unit.t -> bool

(** Tells if a building is hidden by the fog of war *)
val hidden_building : t -> Building.t -> bool

(** @return [true] iff the given unit is visible *)
val visible_unit : t -> Unit.t -> bool

(** @return [true] iff the given building is visible  *)
val visible_building : t -> Building.t -> bool

(** @param fog the fog
  * @param pos the position of unit/building we added
  * @param range the vision range of the unit/building
  * Adds the vision to the fog *)
val add_entity : t -> Position.t -> int -> unit

(** @param fog the fog
  * @param pos the position of the unit/building we removed
  * @param range the vision range of the entity removed
  * Removes the vision of [u] to the fog *)
val remove_entity : t -> Position.t -> int -> unit

(** @param fog the fog
  * @param army a list of units
  * @return the list of visible units in [army] *)
val visible_army : t -> Unit.t list -> Unit.t list

(** @param fog the fog
  * @param buildings a list of buildings
  * @return the list of visible buildings in [buildings] *)
val visible_buildings : t -> Building.t list -> Building.t list
