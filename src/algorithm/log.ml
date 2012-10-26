(* $Id: log.ml 985 2010-06-28 17:28:30Z tolpin $ *)

module P = Pdefinition
module M = Model

type t = { header: unit -> unit;
           algorithm: string -> unit;
           observations: (Loc.opt*float) -> Plan.t -> (Loc.opt*float) -> unit;
           beliefs: Model.t -> unit;
           opinions: (Loc.opt * float list) list -> unit }

let make pdef fname = 
  (* observation log is contiguous for all algorithms in a run *)
  let ofname = fname ^ ".obs"
  
  (* beliefs and opinions are output at the end of each algorithm *)
  and bfname = fname ^ ".bel"
  and pfname = fname ^ ".opn" in

  (* writes to file, safely closing it on exceptions *)
  let write mode writer fname = 
      let ch =
        open_out_gen (mode @ [Open_wronly; Open_text]) 0o666 fname
      in try ( writer ch
             ; close_out ch )
        with e -> ( close_out_noerr ch
                  ; raise e ) in
    
  let append = write [Open_append] 
  and overwrite = write [Open_creat; Open_trunc]
  and write_string string ch = output_string ch (string ^ "\n") (* single line *)
  and write_strings strings ch =              (* tab-separated line of strings *)
    ( output_string ch (String.concat "\t" strings) 
    ; output_string ch "\n" ) in

  (* coordinates: names and values *)
  let crdnames = List.map (fun c -> c.P.c_name) pdef.P.p_coords
  and crdval name idx =
    List.nth (List.find (fun c -> c.P.c_name=name) pdef.P.p_coords).P.c_range idx 
  (* utility and bindings *)
  and mrtnames = pdef.P.p_find.P.m_name::List.map (fun c -> c.P.b_name) pdef.P.p_bindgs in

  (* functions for both header and data: *)
  (* observations log *)
  let oline fnlast fncrd fnobsrv fnacrd fnmerit =
    write_strings 
      ([fnlast "LAST"]
       @ List.map fncrd crdnames
       @ [fnobsrv "OBS"]
       @ List.map fnacrd pdef.P.p_find.P.m_coords
       @ [fnmerit pdef.P.p_find.P.m_name])

  (* beliefs snapshot *)
  and bline fncrd fnftr = 
    write_strings
      (List.map fncrd crdnames
       @ List.map fnftr pdef.P.p_featrs)

  (* opinions snapshot *)
  and pline fncrd fnopn =
    write_strings
      (List.map fncrd pdef.P.p_find.P.m_coords
       @ List.map fnopn mrtnames)

  and asis s = s
  and fixed s _ = s in

  let algorithm aname =
    List.iter (append (write_string ("# " ^ aname))) [ofname; bfname; pfname]

  and header () = 
    (* observation log *)
    ( overwrite
	(oline asis asis asis asis asis)
        ofname
    ; append
        (oline (fixed "0") (fixed "1") (fixed "2") (fixed "3") (fixed "4"))
	ofname

    (* beliefs snapshots *)
    ; overwrite
        (bline asis (fun fname -> fname ^ "_m\t" ^ fname ^ "_v"))
        bfname
    ; append
        (bline (fixed "0") (fixed "1\t2"))
        bfname

    (* merit snapshots *)
    ; overwrite
        (pline asis asis)
        pfname 
    ; append 
        (pline
           (fixed "0")
           (fun mname ->
              if mname=pdef.P.p_find.P.m_name
              then "2"
              else "1"))
        pfname )

  and observations (lprea, mprea) os (la, ma) = 
    (* report a single observation;
       if the observation is not the last one in the plan,
       the alpha and its utility is left unupdated *)
    let observation (iobs, lobs) is_last = 
      append 
	(oline
	   (fixed (if is_last then "1" else "0"))
	   (let coords = List.combine crdnames lobs                  (* make an association list *)
	    in fun cname ->                                  (* and retrieve coordinates by name *)
	      string_of_float (crdval cname (List.assoc cname coords)))
	   (fixed (string_of_int iobs))
	   (let coords = List.combine crdnames (if is_last then la else lprea)
	    in fun cname ->
		match (List.assoc cname coords) with
		    Some idx -> string_of_float (crdval cname idx)
		  | None -> failwith "observations: invalid alpha location")
	   (fixed (string_of_float (if is_last then ma else mprea))))
	ofname in
      
    let rec loop os =
      match os with
	  [o] -> observation o true                         (* the last is last *)
	| o::os -> (observation o false; loop os)
	| _ -> failwith ("observations: call to log with no plan")
    in loop os


  and beliefs modl = 
    modl.M.iter
      (fun lobs bvals ->
         let coords = List.combine crdnames lobs
         and beliefs = List.combine pdef.P.p_featrs bvals
         in append
              (bline 
                 (fun cname -> string_of_float (crdval cname (List.assoc cname coords)))
                 (fun fname ->
                    match List.assoc fname beliefs with
                        (m,v) -> (string_of_float m) ^ "\t" ^ (string_of_float v)))
              bfname)

  and opinions opns =
    (List.iter 
       (fun (lopt, mrtvals) -> 
	  let coords = List.combine crdnames lopt 
	  and merits = List.combine mrtnames mrtvals
	  in append
	       (pline
		  (fun cname ->
		     (match List.assoc cname coords with
			  Some idx -> string_of_float (crdval cname idx)
			| None -> "nan"))
		  (fun mname ->
		     string_of_float (List.assoc mname merits)))
	       pfname)
       opns)

  in { algorithm = algorithm;
       header = header;
       observations = observations;
       beliefs = beliefs;
       opinions = opinions }

  
