(** Launcher of the interface *)

let () = begin

  (new Game.game :> State.state) |> Manager.manager#push ;
  Manager.manager#run

end
