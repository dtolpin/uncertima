(* $Id: log.mli 985 2010-06-28 17:28:30Z tolpin $ *)

type t = { header: unit -> unit;      (* column names in each log file *)
           algorithm: string -> unit; (* mark beginning of a new algorithm *)
           observations:
             (Loc.opt*float)              (* prior alpha *)
             -> Plan.t                           (* plan *)
             -> (Loc.opt*float)       (* posterior alpha *)
             -> unit;
           beliefs: Model.t -> unit;
           opinions: (Loc.opt * float list) list -> unit }

val make: Pdefinition.t -> string (* file name prefix *) -> t
  
