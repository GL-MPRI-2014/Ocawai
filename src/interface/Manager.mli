(** The State Manager *)

val manager : <

  push : State.state -> unit ;
  (** Usage: [push_load load_screen build] where [build] is a state constructor *)
  push_load : State.state -> (unit -> State.state) -> unit ;
  pop : unit ;
  run : unit ;
  window : OcsfmlGraphics.render_window ;
  (* do we really need to put them all ? *)
  current : State.state ;
  event_loop : unit ;
  is_running : bool

>
