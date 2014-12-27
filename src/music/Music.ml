(**
   Music module
*)

type time = Time.t

type 'a t = Note of (time * 'a)
	    | Rest of time

type velocity = int (** Should be between 0 and 127 for MIDI *)

type octave = int
type pitchClass  = Cff | Cf | C | Dff | Cs | Df | Css | D | Eff | Ds
		   | Ef | Fff | Dss | E | Ff | Es | F | Gff | Ess | Fs
		   | Gf | Fss | G | Aff | Gs | Af | Gss | A | Bff | As
		   | Bf | Ass | B | Bs | Bss
type pitch = pitchClass * octave

(**
   The class [param] defines an instantiation for the type ['a t]

   We use a class here for extensivity.
*)
class param pitch velocity = object (self)
  val mutable pitch : pitch = pitch (** The note's pitch *)
  val mutable velocity : velocity = velocity (** The note's velocity *)
				
  method pitch : pitch = pitch
  method velocity : velocity = velocity
				      
  method setPitch : pitch -> unit = fun newPitch ->
    pitch <- newPitch
  method setVelocity : velocity -> unit = fun newVelocity ->
    velocity <- newVelocity
end

type event = param t

let note : time -> 'a -> 'a t = fun dur a ->
  Note(dur, a)
  
let rest : time -> 'a t = fun dur -> Rest (dur)

let duration : 'a t -> time = function
  | Note(dur, _) -> dur
  | Rest(dur) -> dur		 

(** {2 Various utilities} *)

(** {3 Pretty-printing} *)

let rec fprintf : Format.formatter -> event -> unit = fun fmt ->
  function
  | Note(dur, param) ->
    Format.fprintf fmt "@[<1>Note(@,%a,@ %a@,)@]" Time.fprintf dur fprint_param param
  | Rest(dur) -> 
    Format.fprintf fmt "@[<1>Rest(@,%a@,)@]" Time.fprintf dur

and fprint_param : Format.formatter -> param -> unit = fun fmt ->
  function
  | param ->
     let pitch = param#pitch
     and velocity = param#velocity in
     Format.fprintf fmt "@[<1>(pitch =@ %a,@ velocity =@ %d@,)@]"
      fprint_pitch pitch velocity

and fprint_pitch : Format.formatter -> pitch -> unit = fun fmt ->
  function
  | (pitchClass, octave) ->
    Format.fprintf fmt "@[<1>%a%d@]" fprint_pitchClass pitchClass octave

and fprint_pitchClass : Format.formatter -> pitchClass -> unit = fun fmt pitch_class ->
  let pitchClass_name = match pitch_class with
    | Cff -> "Cff" | Cf -> "Cf"   | C -> "C"
    | Dff -> "Dff" | Cs -> "Cs"   | Df -> "Df" 
    | Css -> "Css" | D -> "D"     | Eff -> "Eff" 
    | Ds -> "Ds"   | Ef -> "Ef"   | Fff -> "Fff" 
    | Dss -> "Dss" | E -> "E"     | Ff -> "Ff" 
    | Es -> "Es"   | F -> "F"     | Gff -> "Gff" 
    | Ess -> "Ess" | Fs -> "Fs"   | Gf -> "Gf" 
    | Fss -> "Fss" | G -> "G"     | Aff -> "Aff" 
    | Gs -> "Gs"   | Af -> "Af"   | Gss -> "Gss" 
    | A -> "A"     | Bff -> "Bff" | As -> "As" 
    | Bf -> "Bf"   | Ass -> "Ass" | B -> "B"
    | Bs -> "Bs"   | Bss -> "Bss" 
  in Format.fprintf fmt "@[%s@]" pitchClass_name

let printf = fprintf Format.std_formatter

let pitch_to_string : pitch -> string = function
  | pitch -> fprint_pitch Format.str_formatter pitch;
	     Format.flush_str_formatter ()

(** {3 MIDI conversion} *)

let of_string s =
    if (String.length s < 2) then failwith "Couldn't parse this note.";
    let note = String.sub s 0 (String.length s - 1) in
    let oct = int_of_char s.[String.length s - 1] - int_of_char '0' in
    let off = ref (match s.[0] with
        | 'a' | 'A' -> 0
        | 'b' | 'B' -> 2
        | 'c' | 'C' -> 3
        | 'd' | 'D' -> 5
        | 'e' | 'E' -> 7
        | 'f' | 'F' -> 8
        | 'g' | 'G' -> 10
        | _ -> raise Not_found)
    in
    if String.length s > 2 then begin
      if s.[1] = 's' then incr off
      else if s.[1] = 'f' then decr off;
      if String.length s > 3 then begin
        if s.[2] = 's' then incr off
        else if s.[2] = 'f' then decr off;
      end
    end;
    64 + 12 * (oct - 4) + !off

let toMidi : ?samplerate:int -> ?division:MIDI.division ->
	     ?tempo:Time.Tempo.t -> event -> MIDI.buffer
  = fun ?samplerate:(samplerate = MidiV.samplerate) ?division:(division = MidiV.division)
	?tempo:(tempo = Time.Tempo.base)
  -> function  
  | Rest(duration) -> MIDI.create(MidiV.timeToMidiDuration ~samplerate ~division
							   ~tempo ~duration)
  | Note(duration, param) ->
     (*
        Time.fprintf Format.std_formatter duration;
        print_newline ();
      *)
     let midi_duration = MidiV.timeToMidiDuration ~samplerate ~division
						  ~tempo ~duration
     in
     Printf.printf "%s\n" (pitch_to_string (param#pitch));
     let buffer = MIDI.create(midi_duration)
     and note = of_string (pitch_to_string (param#pitch))
     (** TODO : Requires patching mm.Audio.Note to read sharp
                                 and flat notes *)
     and velocity = MidiV.velocityFromInt (param#velocity)
     in
     (*
        print_int midi_duration;
        print_newline ();
      *)     
     MIDI.insert buffer (0, MIDI.Note_on(note, velocity));
     MIDI.insert buffer (midi_duration-1, MIDI.Note_off(note, velocity));
     buffer
