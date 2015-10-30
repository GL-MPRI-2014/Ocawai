(**
   Modify Module

   Exports a datatype used to indicate additional properties of TPTM,
   e.g. the instrument that should be used to render the given TPTM
 *)

type t = Instrument of Instrument.t

let fprintf fmt : t -> unit = function
  | Instrument (instrument) ->
     Format.fprintf fmt "@[<1>Instrument =@ %a@]" Instrument.fprintf instrument

module Context = struct

  (** This type merges all possible modifiers together in one record *)
  type t =
      { mutable instrument : Instrument.t option
      }

  (** Instrument value extraction 

      Returns the instrument associated with the input [Context.t], returns the default
      instrument if none is provided *) 
  let getInstrument : t -> Instrument.t = fun t ->
    match t.instrument with
    | None -> Instrument.default
    | Some instr -> instr

  (** The empty context, no instrument specified *)
  let empty : unit -> t = fun () ->
    {instrument = None}
	
  let copy : t -> t = fun {instrument = instr} ->
    let t' = empty () in
    t'.instrument <- instr;
    t'

  let compare c1 c2 =
    match (c1.instrument, c2.instrument) with
    | None, None -> 0
    | Some _, None -> 1
    | None, _ -> -1
    | Some instr1, Some instr2 ->
       match Instrument.compare instr1 instr2 with
       | 0 -> 0 (* This shall catch more catches if we add
                 some more modifier types *) 
       | x -> x

end

let replaceContext : t -> Context.t -> unit = fun modifier t ->
  let open Context in
  let aux = function
    | Instrument (instr) -> t.instrument <- Some instr
  in aux modifier

let to_list : Context.t -> t list =fun context -> 
  let open Context in
  let res = ref [] in
  begin
    match context.instrument with
    | None -> ()
    | Some instr -> res := Instrument(instr) :: !res;
  end;
  List.rev !res

let fold_left : ('a -> t -> 'a) -> 'a -> Context.t -> 'a = 
  fun f acc t ->
  let l = to_list t in
  List.fold_left f acc l
		 
