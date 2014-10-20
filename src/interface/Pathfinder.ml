(* We might want to change it later, leave it hidden *)
(* We want to make sure there is no loop and discontinuity in it *)
type t = Position.t list

let init pos = [pos]

(* The empty case is arbitrary and might need to be changed *)
let rec back_to pos = function
  | p :: r when p = pos -> p :: r
  | p :: r -> back_to pos r
  | [] -> [pos]

(* We will change it to handle the case when its not continous *)
let reach path pos =
  if List.mem pos path then back_to pos path
  else pos :: path

let get_move path =
  List.rev path
