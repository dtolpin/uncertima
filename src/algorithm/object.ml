(* $Id: object.ml 985 2010-06-28 17:28:30Z tolpin $ *)

module P = Pdefinition

type t = { probe: Loc.act -> bool;
           observe: Loc.act -> (int * Ndist.t) list }

let ftridx pdef =
  let ftrtab = Pops.enumerate pdef.P.p_featrs in
    fun name ->
      try List.assoc name ftrtab 
      with Not_found -> (* should have been caught in the parser *)
        failwith ("ftridx: undeclared feature: " ^ name)

type simusource = string list (* small enough to keep in memory *)

let hypobs = -1 (* an observation index in the configuration file cannot be -1 *)

type error =
    Unknown_varnam of string
  | Invalid_coltyp of string
  | Wrong_colcount
  | Corrupt_data

exception DataError of error

let from_string = Str.split (Str.regexp "\n+")

let from_channel ich = 
  let rec loop lines =
    try 
      loop 
        (match input_line ich with
             "" -> lines
           | line -> line::lines)
    with End_of_file -> List.rev lines
  in loop [] 

let from_file fname =
  let ich = open_in fname in (* unwind-protect *)
      try 
        let ss = from_channel ich
        in (close_in ich; ss)
      with e -> (close_in ich; raise e)

(* utility types and functions *)

type coltyp = Coord | Mean | Variance
let coltyp_of_string s =
  match s with
      "0" -> Coord
    | "1" -> Mean
    | "2" -> Variance
    |  _  -> raise (DataError (Invalid_coltyp s))

(* match a list of indices with a list of optionial indices:
   lists matches when all indices match;
   index matches an optional index when the optional index is
   missing or the same as the index *)
   
let indices_match =
  List.for_all2 (fun a b ->
                   match (a,b) with
                       (i,None) -> true
                     | (i,Some j) -> i=j)

(* group entries into coordinates, means and variances lookup tables,
   and then turn coordinates table into pattern *)

(* fuzzy assoc to compensate for floating point error *)
let fuzzy_assoc_prec = 1.0E-12
let almost_equal x y = 
  let z = abs_float(x)+.abs_float(y) 
  in z<fuzzy_assoc_prec || abs_float(x-.y)/.z < fuzzy_assoc_prec

let rec fuzzy_assoc value indices = 
  match indices with
      [] -> raise Not_found
    | ((v,i)::indices) when almost_equal value v -> i
    | (_::indices) -> fuzzy_assoc value indices

let group_entries coordinates names types entries =

  (* enumerate coordinates *)
  let crdtab = Pops.tabulate coordinates in
  
  (* build pattern according to the order of coords and values in idxtab *)
  let pattern ct =
    List.map (function (name, indices) ->
                try Some (let value = List.assoc name ct in    
                            try fuzzy_assoc value indices (* translate value to index *)
                            with Not_found -> -1 (* in data but not in problem *))
                with Not_found -> None) (* if coordinate is not given, any location matches *)
      crdtab in

  (* recusrively build lookup tables *)
  let rec loop ns ts xs ct mt vt =
    match ts with 
        [] -> (pattern ct, mt, vt)
      | (t::ts) ->
          match (ns, xs) with
              (n::ns, x::xs) ->
                (match t with
                     Coord    -> loop ns ts xs ((n,x)::ct) mt vt
                   | Mean     -> loop ns ts xs ct ((n,x)::mt) vt
                   | Variance -> loop ns ts xs ct mt ((n,x)::vt))
            | _ -> failwith "group_entries: row lengths differ" (* checked in when_length *)

  in loop names types entries [] [] []

(* Simulator Constructor *)

(* an object simulator need not be fast *)

let simulator pdef source limit =

  let coordinates = pdef.P.p_coords
  and observations = pdef.P.p_obsrvs
  and ftridx = ftridx pdef in

  let split = Str.split (Str.regexp "[ \t]+")         (* split on spaces *) 
  and when_length n l =       (* check that all rows are the same length *)
    if n = List.length l then l
    else raise (DataError Wrong_colcount) in

  (* extract raw typed data *)
  let data =
    match source with (* the source is a list of strings, one string per line *)
        names::types::entries -> 

          (* read names and count columns *)
          let names = split names in
          let rowlen = List.length names in

          (* read types *)
          let types = (when_length rowlen
                         (List.map coltyp_of_string (split types))) in

          (* read and group data *)
          let group = group_entries coordinates names types
          in
            List.map (fun s ->
                        group
                          (when_length rowlen
                             (List.map float_of_string (split s))))
              entries
    
      | _ -> raise (DataError Corrupt_data) in


  (* whether the data are available for a particular location *)
  let available lobs = List.exists (fun (loc,_,_) -> indices_match lobs loc) data in

  (* table of performed observations *)
  let otab = Hashtbl.create ((List.length observations) * (List.length data)) in
    
  let probe = 
    match limit with 
        None ->
          (function (_, lobs) -> available lobs)
      | Some limit ->  
          (function (_,lobs) as o -> 
             available lobs &&
               try
                 (Hashtbl.find otab o) < limit
               with Not_found -> true) in

  let obs_all = (* if iobs=hypobs, return all features; used to apply hypotheses *)
    { P.o_featrs = pdef.P.p_featrs;
      P.o_accurs = List.map (fun _ -> infinity) pdef.P.p_featrs;
      P.o_cost = P.ExCon infinity } in
 
  let observe' (iobs, lobs) =  (* observe when available *)
    (* find a matching location *)
    let (_,ms,vs) =
      try List.find (fun (loc,_,_) -> indices_match lobs loc) data
      with Not_found -> 
	( if iobs <> hypobs then (* for hypothesis it is normal to *)
	    Debug.info "object" ("missing simulation data: ["
				 ^ (String.concat "; " (List.map string_of_int lobs))
				 ^ "]"); ([],[],[]) ) in

    (* retrieve the observation by its index; it defines the list of features to return *)
    let obs =
      try List.assoc iobs observations
      with Not_found -> ( assert (iobs = hypobs) ; obs_all ) in

    (* bind accuracy estimates to features -- in case variances are not specified *)
    let ws = List.map2 (fun a  b -> (a,b)) obs.P.o_featrs obs.P.o_accurs in
      
      (*  and finally package the answer -- a list of observed features *)
      List.map
        (fun name ->
           (ftridx name, 
            Ndist.make
              (try List.assoc name ms
               with Not_found ->                                        (* missing mean -- *)
		 try ( ignore (List.assoc name vs) ; infinity )            (* use nonsense *)
		 with Not_found -> 0.0)                 (* unless variance is also missing *)
              (try List.assoc name vs
               with Not_found -> List.assoc name ws))) (* missing variance -- use estimate *)
        obs.P.o_featrs in     (* answer in the order of features in the problem definition *) 

  (* observe or return unknown evidence if unavailable *)
  let  observe ((iobs, _) as o) =
    match limit with
        None -> observe' o
      | Some limit ->
          let n = try Hashtbl.find otab o with Not_found -> 0 
          in 
            if n < limit then (Hashtbl.add otab o (n+1); observe' o)
            else 
              let obs = List.assoc iobs observations
              in (* unknown evidence *)
                List.map
                  (fun name -> (ftridx name, Ndist.unknown))
                  obs.P.o_featrs

  in { probe = probe;
       observe = observe }

;;

(* tests *) 

let check x = if not x then failwith "indices_match" 
in ( check (indices_match [] [])
   ; check (indices_match [1] [None])
   ; check (indices_match [1] [Some 1])
   ; check (not (indices_match [1] [Some 2]))
   ; check (not (indices_match [1;2] [Some 2; Some 1]))
   ; check (indices_match [1;2] [None; Some 2]))
