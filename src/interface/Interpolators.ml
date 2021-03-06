type interpolator = <
  delete : unit;
  pause  : unit;
  reset  : unit;
  run    : unit;
  dead   : bool
>

let ip_list = ref []

let identifier = ref 0

class interpolator_class func = object(self)

  val mutable origin = Unix.gettimeofday ()

  val mutable last_time = Unix.gettimeofday ()

  val mutable running = true

  val mutable id = !identifier

  val mutable dead = false

  initializer
    incr identifier

  method update t = 
    if running then begin
      func (t -. origin) (t -. last_time);
      last_time <- t
    end

  method id = id

  method delete = 
    dead <- true;
    let rec aux = function
      |[] -> []
      |t::q -> 
        if t#id = self#id then q 
        else t::(aux q)
    in ip_list := aux !ip_list

  method pause = running <- false

  method reset = origin <- (Unix.gettimeofday ())

  method run = running <- true

  method dead = dead

end


class timed_interpolator func lifespan = object(self)

  inherit interpolator_class func as super

  method update t = 
    if t -. origin > lifespan then begin
      super#update (origin +. lifespan);
      self#delete
    end else super#update t

end


let update () = 
  let t = Unix.gettimeofday () in
  List.iter (fun f -> f#update t) !ip_list

let new_ip_from_fun f = 
  let ip = new interpolator_class f in
  ip_list := ip :: !ip_list; 
  (ip :> interpolator)

let new_sine_ip set spe amp med = 
  let ip = new interpolator_class 
    (function t -> function dt -> set (amp *. (sin (spe *. t)) +. med))
  in
  ip_list := ip :: !ip_list;
  (ip :> interpolator)

let new_ip_with_timeout f t = 
  let ip = new timed_interpolator f t in
  ip_list := ip :: !ip_list;
  (ip :> interpolator)


