(**
   TPTM module, defines the high-level functions
   used to create and handle TPTM.
   
   Based on work by P. Hudak, D. Janin and T. Bazin.
   @author "Theis Bazin" @author "Mathias Sable Meyer"
*)

type t

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
val delay : Time.t -> t

(**
   @return an {i idempotent} tile holding [event]
*)
val make : Music.event -> t

(**
   @return a tile holding [event] with duration [dur event]
*)
val make_withDelay : Music.event -> t

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
val inverse : t -> t

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

(** {2 Normalization and rendering functions} *)

(**
   @return a pair [(head, tail)] where
   - [head] holds the first (in terms of the time scale)
   events in [t] and has Pos > 0 unless [tail] holds no events
   - [tail] holds the remaining events in [t] and has Pre = 0

   The following relation holds : [t = head % tail]
*)
val headTail : t -> t * t

(**
   @return a fully normalized tile
   Iterates [TPTM.headTail].
*)
val normalize : t -> t

(** [extract_by_time extract_dur t] splits [t] into [t1] and [t2].
    [t1] has duration [extract_dur] and holds events only
    up to its Pos.
    [t2] holds no events before its Pre,
    and t == t1 % t2

    Actually a partial version normalization, only normalizes
    as long as it's useful. *)
val extract_by_time : Time.t -> t -> t * t

(**
   Convert [t] to MIDI and stream it to the user, freezing the
   current process during playback.
 *)
val play_and_wait : ?samplerate:int -> ?division:MIDI.division ->
		    ?tempo:Time.Tempo.t -> t -> unit 
					 
(**
   Convert [t] to MIDI and stream it to the user, all in another
   forked process, without stopping the current process
 *)
val fork_play : ?samplerate:int -> ?division:MIDI.division ->
		?tempo:Time.Tempo.t -> t -> unit 


(** {2 Testing functions} *) 

(** {3 Tile <-> event list transform} *)

(**
   @return the product of all tiles in the input list
*)
val fromList : t list -> t

(** {3 Pretty_printing} *)

(**
   Pretty prints the input [t] and outputs to the channel
   defined by the [Format.formatter]
*)
val fprintf : Format.formatter -> t -> unit
