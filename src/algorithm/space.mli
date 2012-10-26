(* $Id: space.mli 985 2010-06-28 17:28:30Z tolpin $ *)

(* metric space, coordinates and differences *)

type t = { atobs: unit -> Loc.obs;          (* current observation location *)
           toobs: Loc.obs -> unit;          (* update current *)
           someopt: unit -> Loc.opt;        (* random section *)
           someobs: Loc.opt -> Loc.obs;     (* random location in the section *)
           coord: Loc.obs -> int -> float;  (* coordinate *)
           cdiff: Loc.obs -> int -> float } (* coordinate difference *)

val make: Pdefinition.t -> t
