(* $Id: myopic.mli 985 2010-06-28 17:28:30Z tolpin $ *)

val mkiter: Pdefinition.t -> Object.t -> Plan.mkiter (* for use in derived iterators *)

val make: (Plan.mkiter -> Algo.make) (* algorithm parameterized by iterator *)
  -> Algo.make
