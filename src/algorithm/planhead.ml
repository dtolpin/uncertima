(* $Id: planhead.ml 985 2010-06-28 17:28:30Z tolpin $ *)

module P = Pdefinition
module M = Model
module O = Object
module A = Algo
module L = A.Lib

let debug = Debug.info "planhead"

(* myopic iterator to find the head *)
let byone os =
  let rest = ref os
  in fun () ->
    match !rest with
        [] -> []
      | (h::t) -> (rest := t; [h])

let make mkiter pdef modl spac objt = 

  let _ = debug "make" in

  let { L.eu = eu;
        L.eb = eb;
        L.cost = cost;
        L.alpha = alpha;
        L.bene = bene;
        L.obsupd = obsupd } = L.make pdef modl spac objt
  and lopts = Loc.comprehend 
    (List.map (fun _ -> None) pdef.P.p_coords)     (* all coordinates *)
    (List.map                                 (* in the feature space *)
       (fun c -> 
          if List.mem c.P.c_name pdef.P.p_find.P.m_coords
          then Some (List.length c.P.c_range)
          else None)
       pdef.P.p_coords) in

  (* initialize utilities *)
  let us = ref (List.map eu lopts) in

  let select b = 
    let rec select' next vworst = 
      let rec loop vbest pbest = 
        match (next ()) with 
            [] ->
              ( debug (Printf.sprintf "voi=%g" vbest)
              ; if (List.length pbest) > 1  (* select the plan head: a single best observation *)
                then select' (byone pbest) (-.max_float)           (* even with negative value *)
                else pbest )
          | plan -> 
              let v = (bene plan lopts !us) -. (cost plan)
              in if v > vbest 
                 then loop v plan 
                 else loop vbest pbest 
      in loop vworst []
    in ( Stat.benefit_evaluations :=
	   !Stat.benefit_evaluations +
	     (List.fold_left ( * ) 1 
		(List.map (fun c -> List.length c.P.c_range) pdef.P.p_coords))
       ; select' (mkiter cost b) 0.0 )            (* select best plan with non-negative value *)
         
  and commit b plan = 
    let c = cost plan
    in ( us := obsupd plan lopts !us
       ; b -. c )

  (* for reporting and analysis *)
  and opinions () =
    List.map
      (fun lopt ->
         (lopt, (eu lopt)::List.map (fun (_,i) -> eb i lopt) (Pops.enumerate pdef.P.p_bindgs)))
      lopts

  (* location with maximum utility and its utility *)
  and alpha () = alpha lopts !us

  in let _ = debug "algorithm made"

  in { A.name = "planhead";
       A.select = select;
       A.commit = commit;
       A.opinions = opinions;
       A.alpha = alpha }
