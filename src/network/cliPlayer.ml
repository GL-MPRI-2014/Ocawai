open Unix
open Marshal
open Types

class cliPlayer = 
object (self)
  inherit Player.player
	       


  method get_next_action = 
    (* not my job, but the interface guys' one *)

end

type t = cliPlayer


