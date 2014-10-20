type interpolator = <
  delete : unit;
  pause : unit;
  reset : unit;
  run : unit
>

val update : unit -> unit

val new_ip_from_fun : (float -> unit) -> interpolator


