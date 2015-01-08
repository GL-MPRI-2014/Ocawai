open OcsfmlAudio

exception Unknown_sound of string
exception Not_a_valid_sound_file of string

let sound_bank = Hashtbl.create 101
let path = (Utils.base_path ()) ^ "sounds/"

let volume = ref 100.


module SoundsLog = Log.Make (struct let section = "Sounds" end)

let load_sound_file file_name =
  match (Str.split (Str.regexp "\\.") file_name) with
    | [name; "wav"] ->
        let file = path ^ file_name in
        let buf = new sound_buffer (`File(file)) in
        let sound = new sound ~buffer:buf ~volume:(!volume) () in
        Hashtbl.replace sound_bank name sound;
        SoundsLog.infof "[stored] %s" name;
    | _ ->
        SoundsLog.errorf "%s is not a wav file." file_name;
        raise (Not_a_valid_sound_file (file_name ^ " (only WAV are supported)"))

let load_sounds () =
  let files = Sys.readdir path in
  Array.iter load_sound_file files


let play_sound sound =
    try
      let player = Hashtbl.find sound_bank sound in
      player#play
    with
      | Not_found ->
        SoundsLog.errorf "Couldn't retreive sound %s" sound;
        raise (Unknown_sound sound)

let set_volume f =
  volume := f;
  load_sounds ()


let get_volume () = !volume
