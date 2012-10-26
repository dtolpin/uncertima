(* $Id: remote.mli 811 2010-01-19 22:52:48Z tolpin $ *)

(* XML-RPC based remote object *)

val make: ?noprobe:bool -> Pdefinition.t -> string (* URL of XML-RPC server *) -> Object.t

(* helpers *)
val to_bool: [> `Boolean of bool ] -> bool
val to_string: [> `String of string ] -> string
val to_int: [> `Int of int ] -> int
val to_float: [> `Double of float ] -> float
val to_list: [> `Array of 'a ] -> 'a
