(** The State Manager *)

val manager : <

  push : State.state -> unit ;
  pop : unit ;
  run : unit ;
  (* do we really need to put them all ? *)
  current : State.state ;
  event_loop : unit ;
  is_running : bool

>
