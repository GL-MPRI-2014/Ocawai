(*                              Module Pathfinder                             *)
(*                                                                            *)
(* Its goal is to handle paths, their creation.                               *)

type t

val init : Position.t -> t

val reach : t -> Position.t -> t

val get_move : t -> Action.movement
