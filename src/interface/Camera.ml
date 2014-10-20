class camera ~tile_size ~w ~h ~maxpos = object(self)

  val cursor = new Cursor.cursor ~position:(Position.create (40,40))

  method set_cursor p =
    Position.clamp p (Position.create (0,0)) maxpos
    |> cursor#set_position

  (* For now we hide the objetc cursor but later it might be interesting *)
  (* to give it directly *)
  method cursor = cursor#position

  (* For now, view is centered on the cursor, independently of its position *)
  method project p =
    let (x,y) = Position.project p cursor#position tile_size in
    (x + w/2, y + h/2)

  (* Use this one if you want a view centered on the cursor, except if it
   * is too close from top or left border, so that we never see the
   * "exterior" of the map.
   *
   * NOT WORKING *)

  (*method project p =
    let tleft = Position.diff
      cursor
      (Position.create (w/(2*tile_size) + 1, h/(2*tile_size) + 1)) in
    let (relx,rely) = Position.project tleft cursor tile_size in
    let (offx,offy) = (relx + (w - tile_size)/2, rely + (h - tile_size)/2) in
    let (x,y) = Position.project p self#top_left tile_size in
    (x + offx, y + offy)*)

  method top_left =
    let p = Position.create (w/(2*tile_size) + 1, h/(2*tile_size) + 1) in
    Position.clamp
      (Position.diff cursor#position p)
      (Position.create (0,0))
      maxpos

  method bottom_right =
    let p = Position.create (w/(2*tile_size) + 1, h/(2*tile_size) + 1) in
    Position.clamp
      (Position.add cursor#position p)
      (Position.create (0,0))
      maxpos

  method tile_size = tile_size

end
