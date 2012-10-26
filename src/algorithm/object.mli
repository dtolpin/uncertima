(* $Id: object.mli 985 2010-06-28 17:28:30Z tolpin $ *)

(* object controller and simulator as an implementation
   of the controller *)

(* - an observation is performed in a location,
     location coordinates are numbered in the declaration order;
   - an observation at a location can be probed for availability
     -- used to choose plans *)
type t = 
    { probe: Loc.act -> bool;     (* whether the observation is available *)
      observe: Loc.act -> (int*Ndist.t) list }    (* feature and evidence *)

val ftridx: Pdefinition.t -> Pdefinition.feature -> int

(* the simulator is defined by a textual description *)
type simusource

(* ... and is also used to apply the prior hypothesis *)
val hypobs: int   (* fake observation to retrieve the hypothesis *)

(* error reporting on simulation data format *)
type error =
    Unknown_varnam of string
  | Invalid_coltyp of string
  | Wrong_colcount
  | Corrupt_data

exception DataError of error

(* the data can is read from a source *)
val from_string: string -> simusource
val from_file: string -> simusource
val from_channel: in_channel -> simusource
          
(* ... and are wrapped into an observable object *)
val simulator:
  Pdefinition.t                  
  -> simusource   (* simulator definition *)
  -> int option   (* limit on repetition of the same observation *)
  -> t            (* the object *)
