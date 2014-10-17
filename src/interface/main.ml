open OcsfmlGraphics

let () = begin
  (* Main window *)
  let window = new render_window 
    (OcsfmlWindow.VideoMode.create ~w:800 ~h:600 ())
    "Ocsfml Test" 
  in

  (* Basic event manipulation *)
  let rec event_loop () = 
    match window#poll_event with
    |Some e -> OcsfmlWindow.Event.(
      begin match e with
        | Closed -> window#close
        |  _     -> ()
      end); 
      event_loop ()
    | None  -> ()
  in

  let rec main_loop () =
    if window#is_open then begin
      event_loop ();
      window#clear ();
      (* Rendering goes here *)
      window#display;
      main_loop ()
    end
  in

  main_loop ()
end

