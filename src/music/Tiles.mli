(**
   Tiles module, defines the high-level functions
   used to create and handle TPTM.
   
   Based on work by P. Hudak, D. Janin and T. Bazin.
   @author "Theis Bazin" @author "Mathias Sable Meyer"
*)

type time
type 'a event
type 'a t

(** {2 Base tiles} *)

(**
   Neutral element for (%).
*)
val zero : 'a t

(**
   Compares 'a t to zero
*)
val isZero : 'a t -> bool

(**
   Basic syncing plumbing
*)
val delay : time -> 'a t

(**
   @return an {i idempotent} tile holding [event]
*)
val make : 'a event -> 'a t

(** {2 Tile operators} *)

(**
   Tiled product. An inverse semi-groupe operator.
*)
val (%) : 'a t -> 'a t -> 'a t

(**
   @return [t'] holding the same events as [t]
   only with [Pre t'] = [Pos t'] = [Pre t]
*)
val reset : 'a t -> 'a t


(**
   @return [t'] holding the same events as [t]
   only with [Pos t'] = [Pre t'] = [Pos t]
*)
val coreset : 'a t -> 'a t

(**
   @return [t'] holding the same events as [t]
   only with
   [Pos t'] = [Pre t] and
   [Pre t'] = [Pos t]
*)
val inverse : 'a t -> 'a t

(**
   @return a tile holding both input tiles
   {i starting} together
*)
val fork : 'a t -> 'a t -> 'a t

(**
   @return a tile holding both input tiles
   {i ending} together
*)
val join : 'a t -> 'a t -> 'a t

(** {2 Normalization functions} *)

(**
   @return a pair [(head, tail)] where
   - [head] holds the firt (in terms of the time scale)
   events in [t] and has Pos > 0 unless [tail] holds no events
   - [tail] holds the remaining events in [t] and has Pre = 0

   The following relation holds : [t = head % tail]
*)
val headTail : 'a t -> 'a t * 'a t

(**
   @return a fully normalized tile
   Iterates [headTail].
*)
val normalize : 'a t -> 'a t


