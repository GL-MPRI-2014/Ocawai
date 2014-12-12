(**
  A usefull tool provided by the savonet team that allows to play music on
  pulseaudio. See http://liquidsoap.fm/modules/ocaml-mm/index.html

  @param client_name Name of the pulseaudio client
  @param stream_name Name of the streamed data
  @param channels Number of channels of the audio data
  @param sample_rate Sample rate of the data (usually 44100)
  @return An audio writer, see webpage given above
*)

class writer : string -> string -> int -> int -> Audio.IO.Writer.t
