(**
   Modify Module

   Exports a datatype used to indicate additional properties of TPTM,
   e.g. the instrument that should be used to render the given TPTM
 *)

type t = Instrument of Instrument.t

val fprintf : Format.formatter -> t -> unit

module Context : sig 

  (** This type merges all possible modifiers together in one record *)
  type t =
      { mutable instrument : Instrument.t option
      }

  (** Instrument value extraction 

      Returns the instrument associated with the input [Context.t], returns the default
      instrument if none is provided *) 
  val getInstrument : t -> Instrument.t

  (** The empty context, no instrument specified *)
  val empty : unit -> t

  (** Returns a fresh copy of the input [t] *)
  val copy : t -> t

  (** Compares the two input contexts, derived from the compare function for all members
      of the tuple, with the standard extension from an order on X to an order on X option

      Same specification as [Pervasives.compare]
   *)
  val compare : t -> t -> int
end

(** Replaces the field in [context] corresponding to the input modifier
    with this new value.

    Modifies [context] *)
val replaceContext : t -> Context.t -> unit

(**
   Context folding
 *)
val fold_left : ('a -> t -> 'a) -> 'a -> Context.t -> 'a
