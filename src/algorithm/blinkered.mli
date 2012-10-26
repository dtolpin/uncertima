(* $Id: blinkered.mli 985 2010-06-28 17:28:30Z tolpin $ *)

val make: (Plan.mkiter -> Algo.make) (* algorithm parameterized by iterator *)
  -> Algo.make

