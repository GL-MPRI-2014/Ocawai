(**
   Music module
*)

type time = Time.t

type 'a t = Note of (time * 'a)
	    | Rest of time

type velocity = int (** To-Do : is there a special mm.int
			type for MIDI values in mm ? *)
type octave = int
type pitchClass  = Cff | Cf | C | Dff | Cs | Df | Css | D | Eff | Ds
		   | Ef | Fff | Dss | E | Ff | Es | F | Gff | Ess | Fs
		   | Gf | Fss | G | Aff | Gs | Af | Gss | A | Bff | As
		   | Bf | Ass | B | Bs | Bss
type pitch = pitchClass * octave

type param = pitch * velocity
    
type event = param t

let note : time -> 'a -> 'a t = fun dur a ->
  Note(dur, a)
  
let rest : time -> 'a t = fun dur -> Rest (dur)

let getDur : 'a t -> time = function
  | Note(dur, _) -> dur
  | Rest(dur) -> dur

(** {2 MIDI conversion} *)

(**
To-Do
    
let toMidi : ?samplerate:int -> ?division:MIDI.division -> ~tempo:Time.t -> 'a t -> MIDI.buffer
  = fun ?samplerate:(sr = MidiV.samplerate) ?division:(div = MidiV.division)
  -> function  
  | Rest(dur) -> MIDI.create(Time.toFloat )
  | Music(dur, a) -> 
    let conversion =
      
    in Some conversion
*)

(** {2 Testing functions} *)

(** {3 Pretty-printing} *)

let rec fprintf : Format.formatter -> event -> unit = fun fmt ->
  function
  | Note(dur, param) ->
    Format.fprintf fmt "@[<1>Note(@,%a,@ %a@,)@]" Time.fprintf dur fprint_param param
  | Rest(dur) -> 
    Format.fprintf fmt "@[<1>Rest(@,%a@,)@]" Time.fprintf dur

and fprint_param : Format.formatter -> param -> unit = fun fmt ->
  function
  | (pitch, velocity) ->
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
