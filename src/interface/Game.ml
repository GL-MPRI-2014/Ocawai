class game = object(self)

  inherit State.state as super

  method render window =

    super#render window ;
    Interpolators.update () ;
    window#clear ();

    (* Rendering goes here *)
    (* Render.render_game window cdata; *)
    Render.draw_hud window;

    Printf.printf "Test\n" ;

    (* This is really garbage *)
    (* Render.render_widget window my_menu; *)

    window#display

end
