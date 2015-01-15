(**
   Instrument module

   Exports several instrument types, and their synthesized version
 *)

(**
   The type of instruments
 *)
type t = Kick | Snare | Organ 

let default = Organ

let to_channel : t -> int = function
  | Kick -> 0
  | Snare -> 1
  | Organ -> 2

let compare c1 c2 = match (c1, c2) with
  | Kick, Kick -> 0
  | Kick, _ -> -1
  | Snare, Kick -> 1
  | Snare, Snare -> 0
  | Snare, _ -> -1
  | Organ, Organ -> 0
  | Organ, _ -> 1

let kick_gen samplerate freq v =
  let lpf = new Audio.Mono.Effect.biquad_filter samplerate `Low_pass (freq *. 1.) 2. in
  let adsr = Audio.Mono.Effect.ADSR.make samplerate (0.001, 0.3, 0., 1.) in
  let g = new Audio.Mono.Generator.sine samplerate 80. in
  let g2 = new Audio.Mono.Generator.sine samplerate 90. in
  let g = new Audio.Mono.Generator.mult g g2 in
  let g2 = new Audio.Mono.Generator.white_noise ~volume:0.5 samplerate in
  let g = new Audio.Mono.Generator.add g g2 in
  let g = new Audio.Mono.Generator.chain g lpf in
  let g = new Audio.Mono.Generator.adsr adsr g in
  let g = new Audio.Mono.Generator.chain g (new Audio.Mono.Effect.clip 0.9) in
  let g = new Audio.Mono.Generator.chain g (new Audio.Mono.Effect.amplify 5.) in
  let g = new Audio.Generator.of_mono g in
  g

let snare_gen samplerate freq v =
  let lpf = new Audio.Mono.Effect.biquad_filter samplerate `Low_pass (freq*.5.) 2. in
  let adsr = Audio.Mono.Effect.ADSR.make samplerate (0., 0.25, 0., 1.) in
  let g = new Audio.Mono.Generator.white_noise samplerate in
  let g = new Audio.Mono.Generator.chain g lpf in
  let g = new Audio.Mono.Generator.adsr adsr g in
  let g = new Audio.Generator.of_mono g in
  g

let organ_synth samplerate =
  let adsr = Audio.Mono.Effect.ADSR.make samplerate (0.1, 0.0, 1.0, 0.0) in
      new Synth.create
        (fun f v ->
          new Audio.Generator.of_mono
            (new Audio.Mono.Generator.adsr adsr
              (new Audio.Mono.Generator.add
                (new Audio.Mono.Generator.add
                  (new Audio.Mono.Generator.add
                    (new Audio.Mono.Generator.add
                      (new Audio.Mono.Generator.add
                        (new Audio.Mono.Generator.add
                          (new Audio.Mono.Generator.add
                            (new Audio.Mono.Generator.sine samplerate ~volume:v f)
                            (new Audio.Mono.Generator.sine samplerate ~volume:(v*.1.0) (2.*.f)))
                          (new Audio.Mono.Generator.sine samplerate ~volume:(v*.0.1) (3.*.f)))
                        (new Audio.Mono.Generator.sine samplerate ~volume:(v*.0.2) (4.*.f)))
                      (new Audio.Mono.Generator.sine samplerate ~volume:(v*.0.1) (5.*.f)))
                    (new Audio.Mono.Generator.sine samplerate ~volume:(v*.0.0) (6.*.f)))
                  (new Audio.Mono.Generator.sine samplerate ~volume:(v*.0.0) (7.*.f)))
                (new Audio.Mono.Generator.sine samplerate ~volume:(v*.0.00) (8.*.f)))))

(**
   Returns a synthesizer associated to the inupt [Instrument.t], at the given [samplerate]
 *)
let to_synth ?samplerate:(samplerate = MidiV.samplerate) : t -> Synth.t = function
  | Kick -> new Synth.create @@ kick_gen samplerate
  | Snare -> new Synth.create @@ snare_gen samplerate
  | Organ -> organ_synth samplerate

(**
   Pretty prints the input [event] and outputs to the channel
   defined by the [Format.formatter]
*)
  let fprintf fmt : t -> unit = function
    | Kick -> Format.fprintf fmt "@[Kick@]"
    | Snare -> Format.fprintf fmt "@[Snare@]"
    | Organ -> Format.fprintf fmt "@[Organ@]"
