(**
   TPTM module
*)

open Music

type time = Time.t
type event = Music.event

type t = Tile of DList.t

(** {2 Base tiles} *)

let zero : t = Tile (DList.zero)

let isZero : t -> bool = fun t ->
  t = zero

let delay : time -> t = fun dur ->
  Tile (DList.sync dur)

let make : event -> t = fun event ->
  Tile (DList.return event)

(** {2 Tile manipulation} *)

let (%) : t -> t -> t = fun (Tile(events1)) (Tile(events2)) ->
  Tile (DList.(/::/) events1 events2)

let make_withDelay : Music.event -> t = fun event ->
  make event % delay (Music.duration event) 

let duration : t -> time = function
  | Tile(events) -> DList.duration events

let reset : t -> t = fun t ->
  let dur = duration t in
  let back_delay = delay (Time.inverse dur) in
  t % back_delay

let coreset : t -> t = fun t ->
  let dur = duration t in
  let back_delay = delay (Time.inverse dur) in
  back_delay % t

let inverse : t -> t = fun t ->
  let dur = duration t in
  let back_delay = delay (Time.inverse dur) in
  back_delay % t % back_delay

let join : t -> t -> t = fun t1 t2 ->
  t1 % (coreset t2)

let fork : t -> t -> t = fun t1 t2 ->
  (reset t1) % t2

(** {2 Normalization} *)

let headTail : t -> (t * t) = fun (Tile events) ->
  let (head, tail) = DList.headTail events in
  (Tile head, Tile tail)

let normalize : t -> t = fun (Tile events) ->
  Tile (DList.normalize events)

(** [extract_by_time extract_dur t] splits [t] into [t1] and [t2].
    [t1] has duration [extract_dur] and holds events only
    up to its Pos,
    [t2] holds no events before its Pre,
    and t == t1 % t2

    Actually a partial version of [normalize], only normalizes
    as long as it's useful. *)
let extract_by_time : Time.t -> t -> t * t = fun extract_dur t ->
  let rec aux acc_dur acc_tile = function
    | tile when tile = zero ->
       let missing_dur = Time.minus extract_dur acc_dur in
       (acc_tile % delay missing_dur, delay @@ Time.inverse missing_dur)
    | tile ->
       let (head, tail) = headTail tile in
       let head_dur = duration head in
       let extracted = Time.plus head_dur acc_dur in
       let comp = Time.compare extracted extract_dur in
       match comp with
       | -1 -> aux extracted (acc_tile % head) tail
       | 0  -> (acc_tile % head, tail)
       | +1 ->
	  let (to_extract_limit, limit_to_tail) =
	    (Time.minus extract_dur acc_dur,
	     Time.minus extracted extract_dur) in
	  let new_head = reset head % delay to_extract_limit
	  and new_tail = delay limit_to_tail % tail in
	  (acc_tile % new_head, new_tail)
       | _ -> assert false
  in
  aux Time.zero zero t

let samples_to_seconds ?samplerate:(samplerate = MidiV.samplerate) samples =
  let duration_seconds_num =
    Num.mult_num (Num.num_of_int samples) @@
      Num.div_num (Num.num_of_int 1) (Num.num_of_int samplerate)
  in
  Num.float_of_num duration_seconds_num

let to_MIDI_buffer : ?samplerate:int -> ?division:MIDI.division ->
		?tempo:Time.Tempo.t -> t -> MIDI.Multitrack.buffer =
  fun ?samplerate:(samplerate = MidiV.samplerate) ?division:(division = MidiV.division)
      ?tempo:(tempo = Time.Tempo.base) ->
  fun t ->
  let Tile(dl) = normalize t in
  let events = DList.toMidi ~samplerate ~division ~tempo dl in
  match events with
  | None -> MIDI.Multitrack.create 16 0
  | Some(buf) -> 
     let duration = MIDI.Multitrack.duration [|buf|] in
     let multi_track = MIDI.Multitrack.create 16 duration in
     multi_track.(0) <- buf;
     multi_track

let play : ?samplerate:int -> ?division:MIDI.division ->
		?tempo:Time.Tempo.t -> t -> unit =
  fun ?samplerate:(samplerate = MidiV.samplerate) ?division:(division = MidiV.division)
      ?tempo:(tempo = Time.Tempo.base) ->
  fun t ->
  let midi_player = new MidiPlayer.asynchronousMidiPlayer in
  let buffer = to_MIDI_buffer ~samplerate ~division ~tempo t in
  let duration = MIDI.Multitrack.duration buffer in
  let duration_seconds = samples_to_seconds duration in
  midi_player#add buffer;
  let killer () =
    Thread.delay duration_seconds;
    midi_player#stop ()
  in
  ignore @@ Thread.create midi_player#play ();
  ignore @@ Thread.create killer ()
			      
(** {2 Testing functions} *)

(** {3 Tile <-> event list transform} *)

let fromList : t list -> t = function list ->
  List.fold_left (%) zero list

(** {3 Pretty-printing} *)

let rec fprintf : Format.formatter -> t -> unit = fun fmt -> function
  | Tile(t) -> Format.fprintf fmt "@[Tile(%a@,)@]@." DList.fprintf t
  
let rec printf : t -> unit = fprintf Format.std_formatter
