(** Launcher of the interface *)

open Manager

(* Helps compiling doc -- to be removed ! *)
open TPTM

let () = begin

  (* (new Game.game :> State.state) |> Manager.manager#push ; *)
  (new MainMenu.main_menu :> State.state) |> manager#push ;
  manager#run;
  Config.config#save_all
end
