(** Launcher of the interface *)
open Logics
open Manager

(* Helps compiling doc -- to be removed ! *)
open Logics

let () = begin

  (* (new Game.game :> State.state) |> Manager.manager#push ; *)
  (new MainMenu.main_menu :> State.state) |> manager#push ;
  manager#run

end
