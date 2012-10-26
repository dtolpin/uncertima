(* $Id: hypothesis.ml 985 2010-06-28 17:28:30Z tolpin $ *)

module M = Model
module O = Object

(* The hypothesis file format is the same as the object simulator
   data format; observation Object.hypobs retrieves beliefs for
   all features.

   If the hypothesis does not contain data for a particular location,
   `Object.observe' returns an empty list, and the location is simply
   not updated. *)

let apply pdef modl hypfname =
  let objt = O.simulator pdef (O.from_file hypfname) None
  in ( modl.M.iter (fun lobs _ ->
                      List.iter 
                        (fun (ftr, e) -> modl.M.update lobs ftr e true)
                        (objt.O.observe (O.hypobs, lobs)))
     ; modl.M.propagate ()
     ; modl.M.commit () )
         
