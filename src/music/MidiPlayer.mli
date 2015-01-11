(**
   MidiPlayer module used to play midi buffer generated elsewhere.

   mm midi's buffers are used as data bricks, as they are added and played.
 
   @author Mathias Sable Meyer
*)

(** Volume is here an int from 0 to 100*)
val get_volume : unit -> int
val set_volume : int -> unit

(**
  The class of the midi player that has {b shiny} buffer behaviour.

  This object is supposed to behave this way : you add some initial buffers,
  then start playing music in the background (Thread.create x#play ()), then add
  even more amazing music whenever you actually have it, and it plays everything
  seamlessly.
*)
class asynchronousMidiPlayer :
  object 

    (** Add a buffer at the end of the main buffer
      * @param buf Buffer to be added to the main buffer and played later *)
    method add : MIDI.Multitrack.buffer -> unit

    (** Loops and play the main buffer *)
    method play : unit -> unit

    (** Gives you information on how long, in buffer size, the player can still
        output music *)
    method remaining : unit -> int

    (** Stop playing properly *)
    method stop : unit -> unit

  end


(**
  A simple way to play a midi file, with simple samples.

  Should diseapear in the long run, but is still used for testing purposes,
  and of course for the amazing snake/tetris music.

  @param path Path to the MIDI file
  @param should_run When this bool ref is set to false, it stops playing
*)
val play_midi_file : string -> bool ref -> unit
