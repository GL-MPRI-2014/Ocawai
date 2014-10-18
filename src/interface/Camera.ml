class camera ~tile_size ~w ~h ~maxpos = object(self)

  val mutable cursor = Position.create (4,4)

  method set_cursor p = 
    cursor <- Position.clamp p (Position.create (0,0)) maxpos

  method cursor = cursor

  (* For now, view is centered on the cursor, independantly of its position *)
  method project p = 
    let (x,y) = Position.project p cursor tile_size in
    (x + (w - tile_size)/2, y + (h - tile_size)/2)

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
      (Position.diff cursor p)
      (Position.create (0,0))
      maxpos

  method bottom_right = 
    let p = Position.create (w/(2*tile_size) + 1, h/(2*tile_size) + 1) in
    Position.clamp 
      (Position.add cursor p)
      (Position.create (0,0))
      maxpos

  method tile_size = tile_size

end


  
