(* $Id: mk_s.ml 985 2010-06-28 17:28:30Z tolpin $ *)

(* simulate simulation data -- for testing *)

open Pdefinition

let mk_s
    pname                  (* problem definition file name *)
    sname                  (* simulator data file name *)
    cnames_opt             (* coordinate names to tabulate on or all if None *)
    fsigs_opt              (* signal functions on coordinates for every features or None *) 
    fmeans_opt             (* noise distributions for mean of every feature or (0.0, 1.0) *)
    fvars_opt =            (* noise distributions for variance of every feature
                              or (mean variance, 0.0) *)
  let pdef = Pops.parse pname in
  let cnames =
    match cnames_opt with
        Some cnames -> cnames
      | None -> List.map (fun c -> c.c_name) pdef.p_coords in
  let fsigs =
    match fsigs_opt with
        Some fsigs -> fsigs
      | None -> List.map (fun _ -> fun _ -> 0.0) pdef.p_featrs in
  let fmeans =
    match fmeans_opt with
        Some fmeans -> List.map (fun (m,v) -> Ndist.make m v) fmeans
      | None -> List.map (fun _ -> Ndist.make 0.0 1.0) pdef.p_featrs in
  let fvars =
    match fvars_opt with
        Some fvars -> List.map (fun (m,v) -> Ndist.make m v) fvars
      | None -> List.map (fun (m,v) -> Ndist.make v 0.0) fmeans in
  let mean_sample ctuple fsig dist = (fsig ctuple)+.(Ndist.sample dist) in
  let var_sample dist = let x = Ndist.sample dist in abs_float(x) in
  let rec loc coords ctuple = 
    match coords with 
        [] -> let ctuple = List.rev ctuple in
          [ctuple
           @ (List.map2 (mean_sample ctuple) fsigs fmeans)
           @ (List.map var_sample fvars)]
      | coord::coords when List.mem coord.c_name cnames ->
          List.fold_right (@)
            (List.map (fun cvalue -> loc coords (cvalue::ctuple)) coord.c_range) []
      | _::coords -> loc coords ctuple in
  let och = open_out sname in
    ( output_string och ((String.concat "\t" (cnames @ pdef.p_featrs @ pdef.p_featrs)) ^ "\n")
    ; output_string och ((String.concat "\t"
                            ((List.map (fun _ -> "0") cnames) 
                             @ (List.map (fun _ -> "1") pdef.p_featrs)
                             @ (List.map (fun _ -> "2") pdef.p_featrs))) ^ "\n")
    ; List.iter (fun l ->
                   output_string och ((String.concat "\t" (List.map string_of_float l)) ^ "\n"))
      (loc pdef.p_coords [])
    ; close_out och )

(* examples *)
let mk_svm_s_0 pname sname =
  mk_s pname sname
    None                                          (* all coordinates *)
    (Some [function
               [c;gamma] -> abs_float(c-.gamma)   (* signal falls towards the diagonals *)
             | _ -> infinity ])                 
    (Some [(1.0, 4.0)])                           (* mean noise mean=1.0, variance=4.0 *)
    (Some [(4.0, 0.5)])                           (* varying accuracy mean=4.0, variance=0.5 *)

let mk_svm_s_C_0 pname sname =
  mk_s pname sname
    (Some ["C"])                                  (* only C *)
    (Some [function
               [c] -> 16. /. (1.+.abs_float(c))   (* signal high close to zero *)
             | _ -> infinity])
    (Some [(1.0, 4.0)])                           (* mean noise mean=1.0, variance=4.0 *)
    None                                          (* fixed variance *)

let mk_svm_s_gamma_0 pname sname =
  mk_s pname sname
    (Some ["gamma"])                              (* only gamma *)
    (Some [function
               [gamma] -> tanh gamma              (* signal correlates with gamma *)
             | _ -> infinity])
    None                                          (* default noise mean (0.0,1.0) *)
    (Some [(4.0,0.0)])                            (* fixed accuracy 4.0 *)
