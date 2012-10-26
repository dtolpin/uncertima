(* $Id: init.ml 985 2010-06-28 17:28:30Z tolpin $ *)

open Error

let seed_random_source () =
  try 
    let rinp = try open_in_bin "/dev/urandom" with x -> open_in_bin "/dev/random"
    in ( Random.init (input_binary_int rinp)
       ; close_in rinp )
  with _ -> error "failed to seed Random from random source"

