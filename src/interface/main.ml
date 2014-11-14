(** Launcher of the interface *)
open Manager
open TPTM

(* Helps compiling doc -- to be removed ! *)
open Logics
open Theme
open Settings
open Setters

let () = begin

  (* (new Game.game :> State.state) |> Manager.manager#push ; *)
  (new MainMenu.main_menu :> State.state) |> manager#push ;
  manager#run

end
