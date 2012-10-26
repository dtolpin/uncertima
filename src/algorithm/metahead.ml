module P = Pdefinition
module M = Model
module O = Object
module A = Algo
module L = A.Lib
module S = Space

let debug = Debug.info "metahead"

let make mkiter pdef modl spac objt = 

  let _ = debug "make" in

  let { L.eu = eu;
        L.eb = eb;
        L.cost = cost;
        L.alpha = alpha;
        L.bene = bene;
        L.obsupd = obsupd } = L.make pdef modl spac objt in

  let every = List.map (fun _ -> None) pdef.P.p_coords in
  let lopts = Loc.comprehend every
    (List.map                                 (* in the feature space *)
       (fun c -> 
          if List.mem c.P.c_name pdef.P.p_find.P.m_coords
          then Some (List.length c.P.c_range)
          else None)
       pdef.P.p_coords) 
  and lacts = 
    let lobss = List.map spac.S.someobs
      (Loc.comprehend every
         (List.map
            (fun c -> Some (List.length c.P.c_range))
            pdef.P.p_coords))
    and iobss = (List.map fst pdef.P.p_obsrvs)
    in List.concat
         (List.map (fun lobs -> (List.map (fun iobs -> (iobs,lobs)) iobss))
            lobss) in

  (* cost from location of first measurement*)
  let mcost plan =
    let hp = [List.hd plan]
    in cost (hp @ plan) -. cost hp in

  (* benefit uncertainty per unit time *)
  let tt = ref 0.0                            (* uncertainty per cost unit: tt = tau^2 *)
  and nt = ref 0.0                               (* number of terms used to compute tt *)
  and cm =                    (* measurement cost of last action, for updating beliefs *)
    ref (List.fold_left max 0.0 (List.map (fun a -> mcost [a]) lacts)) in

  (* initialize utilities ... *)
  let us = ref (List.map eu lopts) 
  (* ... and benefits *)
  and bes = ref (List.map (fun _ -> (Ndist.unknown, !cm)) lacts) in

  (* benefit of re-computing benefit *)
  let benebe be_m be_v gamma = 
    let db = abs_float (be_m-.gamma)
    in  0.3989422804014327 (* 1./sqrt(2*.pi) *)
        *. (sqrt be_v) *. exp(-. db *. db /. 2. /. be_v)
        -. db *. Ndist.cdf (0.0, be_v) (-. db) in

  (* benefit of action according to semi-myopic scheme *)
  let bene' next action = 
    let rec loop be =                                   (* over all plans in the scheme *)
      match (next ()) with
          [] -> be
        | plan -> 
            loop 
              (if List.mem action plan then                    (* if action in the plan *)
                 max be (bene plan lopts !us)  (*   compute benefit and update estimate *)
               else be)
    in ( Stat.benefit_evaluations := !Stat.benefit_evaluations+1
       ; loop 0.0 ) in

  (* compute and update the benefit of an action, return the new benefit *)
  let update budget a' = 
    let be_m' = ref 0.0
    in ( bes := List.map2 (fun a (((be_m, be_v), age) as be) ->
			     if a=a' then 
			       ( be_m' := bene' (mkiter cost budget) a
			       ; (if age > 0. then
				    let dbe = !be_m' -. be_m
				    in ( tt := (!tt *. !nt +. dbe *. dbe /. age)
				       ; nt := !nt +. 1.
				       ; tt := !tt /. !nt
				       ; (Ndist.make !be_m' 0., 0.) )
				  else be) )
			     else be) lacts !bes
       ; !be_m' ) in
  
  let select budget = 
    (* select the best plan based on refreshed benefits of each action *)
    let rec best_plan lacts bes vbest pbest =
      match (lacts, bes) with
          ([],[]) ->
	    (* check that VOI of the selected action is positive,
	       otherwise stop *)
	    let vbest = match pbest with 
		[] -> vbest
	      | [a] -> update budget a -. cost [a] 
	      | _ -> failwith "best_plan: multiple actions" in
	    let pbest = if vbest > 0.0 then pbest else []
	    in ( debug (Printf.sprintf "voi=%g" vbest)
	       ; pbest )
        | (a::lacts, ((be_m,_),_)::bes) -> 
            ( let v = be_m -. (cost [a])
              in 
                if v > vbest 
                then best_plan lacts bes v [a]
                else best_plan lacts bes vbest pbest )
        | _ -> failwith "select: lengths differ" in

    (* refresh benefits *)
    let rec refresh () =
      (* compute alpha and beta for benefits *)
      let rec loop bes alpha beta =
        match bes with
            [] -> discover alpha beta
          | ((be_m,_),_)::bes ->
              if be_m > alpha then loop bes be_m alpha
              else if be_m > beta then loop bes alpha be_m
              else loop bes alpha beta
      in loop !bes 0.0 0.0

    (* discover outdated benefit values,
       re-invokes refresh if a benefit is worth recomputing *)
    and discover alpha beta = 
        let rec loop lacts bes vbest abest =
          match (lacts,bes) with
              ([],[]) -> 
		let _ = debug (Printf.sprintf "vbest=%g" vbest) in
                if vbest > 0.0 then
		  ( ignore (update budget abest)
		  ; refresh () )
                else ()
            | (a::lacts, ((be_m, be_v) as be_dist, _)::bes) ->
                if not (Ndist.known be_dist) then  (* VOIs are unknown before the first measurement *)
		  ( ignore (update budget a)          (* and are computed from the initials beliefs *)
      	          ; refresh () )
                else
                  let v = ( if be_v > 0.0
                            then benebe be_m be_v (if be_m > beta then beta else alpha) 
                            else 0.0 ) -. !Conf.benefit_cost
                  in
                    if v > vbest
                    then loop lacts bes v a
                    else loop lacts bes vbest abest
            | _ -> failwith "refresh: lengths differ"
	in ( loop lacts !bes 0.0 (-1,[]) )

    in ( refresh () ; best_plan lacts !bes 0.0 [] )
         
  and commit budget plan = 
    let budget = budget -. cost plan
    in ( us := obsupd plan lopts !us
       ; cm := mcost plan
       ; bes := (List.map2 (fun act ((be_m, be_v), age) -> 
                              if cost [act] <= budget
                              then (Ndist.make be_m (be_v +. !cm *. !tt), age +. !cm)
                              else (Ndist.make 0. 0., 0.))
                   lacts !bes)
       ; ignore (update budget (List.hd plan))
       ; debug (Printf.sprintf "tt=%g" !tt)
       ; budget )

  (* for reporting and analysis *)
  and opinions () =
    List.map
      (fun lopt ->
         (lopt, (eu lopt)::List.map (fun (_,i) -> eb i lopt) (Pops.enumerate pdef.P.p_bindgs)))
      lopts
      
  (* location with maximum utility and its utility *) 
  and alpha () = alpha lopts !us

  in let _ = debug "algorithm made"

  in { A.name = "metahead";
       A.select = select;
       A.commit = commit;
       A.opinions = opinions;
       A.alpha = alpha }
