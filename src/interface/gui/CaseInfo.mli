(** About showing case info on the screen *)

(** Prints tile/building/unit information *)
class case_info : object

  (** @param target the render target
    * @param drawer a function to print a sprite (to avoid circular build)
    * @param tile_drawer function to draw a tile (likewise)
    * @param damage estimation of the damage (if attacking)
    * @param u the selected unit
    * @param chara a string representing the character of the unit
    * @param building the selected building
    * @param b_chara the character (string) owning the building
    * @param tile the selected tile
    * Draws the information related to the selection *)
  method draw : #OcsfmlGraphics.render_target ->
    (string -> (float * float) -> unit) ->
    (string -> (float * float) -> unit) ->
    (int * int) option ->
    Unit.t option ->
    string ->
    Building.t option ->
    string ->
    Tile.t ->
    unit

end
