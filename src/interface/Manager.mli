(** The State Manager *)

(** The manager in itself *)
val manager : <

  (** Pushes a state on the stack to become the current *)
  push : State.state -> unit ;
  (** Removes the current state from the stack, restores the previous one *)
  pop : unit ;
  (** The method that is called every frame, runs the current state *)
  run : unit ;
  (** @Return the current window *)
  window : OcsfmlGraphics.render_window ;
  (** Resets the window (for resizing) *)
  reset_window : unit ;
  (** Toogles fullscreen mode *)
  set_fullscreen : bool -> unit ;
  (** @Return the state on top of the stack *)
  current : State.state ;
  (** Processes all events *)
  event_loop : unit ;
  (** States if the program is still running, if false, it will terminate *)
  is_running : bool

>
