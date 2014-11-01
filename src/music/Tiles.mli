(**
   Tiles module, defines the high-level functions
   used to create and handle TPTM.
   
   Based on work by P. Hudak, D. Janin and T. Bazin.
   @author "Theis Bazin" @author "Mathias Sable Meyer"
*)

type t
type time
type event

(** {2 Base tiles} *)

(**
   Neutral element for (%).
*)
val zero : t

(**
   Compares t to zero
*)
val isZero : t -> bool

(**
   Basic syncing plumbing
*)
val delay : time -> t

(**
   @return an {i idempotent} tile holding [event]
*)
val make : event -> t

(** {2 Tile operators} *)

(**
   Tiled product. An inverse semi-groupe operator.
*)
val (%) : t -> t -> t

(**
   @return [t'] holding the same events as [t]
   only with [Pre t'] = [Pos t'] = [Pre t]
*)
val reset : t -> t


(**
   @return [t'] holding the same events as [t]
   only with [Pos t'] = [Pre t'] = [Pos t]
*)
val coreset : t -> t

(**
   @return [t'] holding the same events as [t]
   only with
   [Pos t'] = [Pre t] and
   [Pre t'] = [Pos t]
*)
val coreset : t -> t

(**
   @return a tile holding both input tiles
   {i starting} together
*)
val fork : t -> t -> t

(**
   @return a tile holding both input tiles
   {i ending} together
*)
val join : t -> t -> t

(** {2 Normalization functions} *)

(**
   @return a pair [(head, tail)] where
   - [head] holds the firt (in terms of the time scale)
   events in [t] and has Pos > 0 unless [tail] holds no events
   - [tail] holds the remaining events in [t] and has Pre = 0

   The following relation holds : [t = head % tail]
*)
val headTail : t -> t * t

(**
   @return a fully normalized tile
   Iterates [headTail].
*)
val normalize : t -> t


