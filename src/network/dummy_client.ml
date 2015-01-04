
let socket = Network_tool.open_connection "127.0.0.1" 1234

let dealer = Dealer.create_dealer socket

let dummy_mutex = Mutex.create ()

let () = dealer#run dummy_mutex
