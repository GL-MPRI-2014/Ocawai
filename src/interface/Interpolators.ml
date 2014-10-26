type interpolator = <
  delete : unit;
  pause  : unit;
  reset  : unit;
  run    : unit
>

let ip_list = ref []

let identifier = ref 0

class interpolator_class func = object(self)

  val mutable origin = Unix.gettimeofday ()

  val mutable running = true

  val mutable id = !identifier

  initializer
    incr identifier

  method update t = 
    if running then func (t -. origin)

  method id = id

  method delete = 
    let rec aux = function
      |[] -> []
      |t::q -> 
        if t#id = self#id then q 
        else t::(aux q)
    in ip_list := aux !ip_list

  method pause = running <- false

  method reset = origin <- (Unix.gettimeofday ())

  method run = running <- true

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

let new_ip_with_timeout f t = 
  let ip = new timed_interpolator f t in
  ip_list := ip :: !ip_list;
  (ip :> interpolator)


