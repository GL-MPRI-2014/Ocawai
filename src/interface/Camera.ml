open Utils

class camera ~def_tile_size ~w ~h ~maxpos = object(self)

  val cursor = new Cursor.cursor ~position:(Position.create (40,40))

  val mutable offset = (0, 0)

  val mutable actual_interpolator = None

  val mutable zoom_factor = 1.

  method cursor = cursor

  method project p =
    let (x,y) = Position.topair (Position.diff p cursor#position) in
    let (dx,dy) = (x * self#tile_size + (fst offset),
                   y * self#tile_size + (snd offset)) in
    (dx + w/2, dy + h/2)

  method top_left =
    let p = Position.create
      ((w + (fst offset * 2))/(2*self#tile_size) + 1,
       (h + (snd offset * 2))/(2*self#tile_size) + 1) in
    Position.clamp
      (Position.diff cursor#position p)
      (Position.create (0,0))
      maxpos

  method bottom_right =
    let p = Position.create
      ((w - (fst offset * 2))/(2*self#tile_size) + 1,
       (h - (snd offset * 2))/(2*self#tile_size) + 1) in
    Position.clamp
      (Position.add cursor#position p)
      (Position.create (0,0))
      maxpos

  method tile_size = int_of_float (float_of_int def_tile_size *. zoom_factor)

  method move displ =
    let new_position = Position.clamp
      (Position.add (Position.create displ) cursor#position)
      (Position.create (0,0))
      maxpos
    in
    let (dx, dy) = Position.topair
      (Position.diff new_position cursor#position)
    in
    let (offx, offy) = foi2D (dx * self#tile_size, dy * self#tile_size) in
    offset <- iof2D (offx, offy);
    let interp_function t =
      offset <- iof2D (offx *. (1. -. t *. 10.), offy *. (1. -. t *. 10.))
    in
    actual_interpolator >? (fun i -> i#delete);
    actual_interpolator <- Some(
      Interpolators.new_ip_with_timeout interp_function 0.1);
    cursor#set_position new_position

  method set_position pos =
    let clamped = Position.clamp pos (Position.create (0,0)) maxpos in
    self#move (Position.topair (Position.diff clamped cursor#position))

  method zoom = zoom_factor

  method set_zoom z = zoom_factor <- min (max z 0.1) 2.5

end
