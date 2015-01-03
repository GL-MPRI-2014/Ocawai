(** Interpolators for animation *)

(** Type of an interpolator *)
type interpolator = <
  delete : unit;
  pause : unit;
  reset : unit;
  run : unit;
  dead : bool
>

(** Called to update all interpolators *)
val update : unit -> unit

(** Creates a new interpolator from its function *)
val new_ip_from_fun : (float -> float -> unit) -> interpolator

(** [new_sine_ip set speed amp med] creates a new sinusoidal interpolator
 * calling [set], of amplitude [amp], offset [med] and period 2*pi/[spe] *)
val new_sine_ip : (float -> unit) -> float -> float -> float -> interpolator

val new_ip_with_timeout : (float -> float -> unit) -> float -> interpolator
