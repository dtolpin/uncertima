(* $Id: model.ml 985 2010-06-28 17:28:30Z tolpin $ *)

module P = Pdefinition

type t = { fetch: Loc.section -> int -> Ndist.t list;
           update: Loc.obs -> int -> Ndist.t -> bool -> unit;
           propagate: unit -> unit;
           commit: unit -> unit;
           rollback: unit -> unit;
           modified: Loc.opt -> bool;
           modification_rate: unit -> float;
           iter: (Loc.obs -> Ndist.t list -> unit) -> unit }

(* current beliefs are stored in a matrix *)

(* every cell of the matrix contains the current belief, the backup
   of the last commit, and a flag whether the cell was touched 
   (and backed up) by the update *)
type cell = { point: Loc.obs;              (* location of this cell, for reflection *)
              beliefs: Ndist.t array;      (* current beliefs *)
              backups: Ndist.t array;      (* backed up beliefs *)
              mutable touched: bool }      (* true when backed up *)

type matrix =
    Matrix of matrix array    (* multidimensional array *)
  | Cell of cell

(* construct the matrix *)
let make_matrix coordinates nfeatures =
  let rec loop crds tniop =
    match crds with
        [] -> 
          let point = List.rev tniop in
          let beliefs = Array.make nfeatures Ndist.unknown in
            Cell { beliefs = beliefs;
                   backups = Array.copy beliefs;
                   touched = false;
                   point = point }
      | {P.c_range = range}::crds ->
          Matrix (Array.of_list (List.map (fun (_,i) -> loop crds (i::tniop))
                                   (Pops.enumerate range)))
  in loop coordinates []

(* universal OR accessor *)
let access matrix crds_opt combine f =
  let rec loop matrix crds_opt =
    match (matrix,crds_opt) with
        (Cell c,[]) -> f c (* if leaf, apply the function *)
      | (Matrix matrices, crd_opt::crds_opt) -> 
          ( match crd_opt with
                Some crd -> (* the coordinate is specified *)
                  loop (matrices.(crd)) crds_opt
              | None -> (* if not specified, combine multiple values *)
                  combine
                    (List.map (fun m -> loop m crds_opt)
                       (Array.to_list matrices)) )
      | _ -> failwith "access: length crds_opt != depth matrix"
  in loop matrix crds_opt 

(* access forall *)
let forall matrix combine f = 
  let rec loop matrix =
    match matrix with
        Cell c -> f c
      | Matrix matrices -> 
          combine
            (List.map (fun m -> loop m)
               (Array.to_list matrices))
  in loop matrix

(* whether the two beliefs differ *)
let beliefs_differ bold bnew =
  not (Ndist.known bold) ||
    let (mo,vo) = bold and (mn,vn) = bnew in
    let dm = mo -. mn and sm = mo +. mn
    and dv = sqrt(vo) -. sqrt(vn) and sv = sqrt(vo) +. sqrt(vn)
    in sqrt(dm *. dm +. dv *. dv) >=
         !Conf.belief_update_precision *. sqrt(sm *. sm +. sv *. sv)

(* modify a matrix cell blending the update with the current belief,
   return true iff the belief has been updated *)
let modify matrix crds ftr upd blend = 
    let rec loop matrix crds =
      match (matrix,crds) with
          (Cell c,[]) ->
            let bold = c.beliefs.(ftr) in
            let bnew = blend bold upd
            in beliefs_differ bold bnew &&
                 ( if not c.touched then
                     ( Array.blit c.beliefs 0 c.backups 0 (Array.length c.beliefs)
                     ; c.touched <- true ) 
                 ; c.beliefs.(ftr) <- bnew 
                 ; true )
        | (Matrix matrices, crd::crds) -> loop matrices.(crd) crds
        | _ -> failwith "modify: length crds != depth matrix"
    in loop matrix crds

(* the matrix is augmented with a dependency network;
   the network establishes probabilistic relations
   between  matrix cells, provides initial estimates
   and used to propagate the updates *)
type network = { (* return prior belief for a point in the grid *)
                 n_matrix: matrix;
                 
                 (* notify of new evidence *)
                 n_notify: Loc.obs -> int -> Ndist.t -> unit;
 
                 (* propagate beliefs through the grid *)
                 n_propagate: unit -> unit }

(* builds model methods on top of the dependency structure *)
let make_model { n_matrix = matrix;
                 n_notify = notify;
                 n_propagate = propagate } =

  let fetch crds_opt ftr =
    access matrix crds_opt List.concat (fun c -> [c.beliefs.(ftr)])

  and update crds ftr evd real (* or simulated? *) =
    let blend (m,v) (me,ve) = Ndist.make ((me*.v+.m*.ve)/.(v+.ve)) (v*.ve/.(v+.ve))
    in ( if real then notify crds ftr evd
       ; ignore (modify matrix crds ftr evd blend) )

  and modified crds_opt = 
    access matrix crds_opt (List.fold_left (||) false) (fun c -> c.touched)

  and modification_rate () =
    (forall matrix (List.fold_left (+.) 0.) (fun c -> if c.touched then 1. else 0.))
    /. (forall matrix (List.fold_left (+.) 0.) (fun _ -> 1.))

  and commit () = (* clear the modification bit *)
    forall matrix (fun _ -> ()) (fun c -> c.touched <- false)

  and rollback () = (* copy back the backups *)
    forall matrix (fun _ -> ())
      (fun c -> 
         if c.touched then
           ( c.touched <- false
           ; Array.blit c.backups 0 c.beliefs 0 (Array.length c.backups) ))

  and iter f =
    forall matrix (fun _ -> ()) (fun c -> f c.point (Array.to_list c.beliefs))
 
  in ( commit () (* forget all preparatory changes *)
     ; { fetch = fetch;
         update = update;
         propagate = propagate;
         commit = commit;
         rollback = rollback;
         modified = modified;
         modification_rate = modification_rate;
         iter = iter } )

(* lattice -- rectangluar network with uniform dependencies:
   -x--x--x--x--x--x-
    |  |  |  |  |  |
   -x--x--x--x--x--x-
    |  |  |  |  |  | 
   -x--x--x--x--x--x- *)
   
let lattice pdef mdef =
  let matrix = make_matrix pdef.P.p_coords (List.length pdef.P.p_featrs)
  and limits = List.map (fun c -> List.length c.P.c_range) pdef.P.p_coords
  and ftrtab = Pops.enumerate pdef.P.p_featrs in

  let nodes = forall matrix List.concat (fun c -> [c.point])

  (* find neighbors: adjacent nodes if there is a connection along the coordinate *)
  and neighbors crds = 
    let rec loop head tail limits cdefs neighs =
      match (tail,limits,cdefs) with 
          ([],[],[]) -> neighs
        | (crd::tail,lim::limits, {P.c_name = cname }::cdefs) -> 
            loop (crd::head) tail limits cdefs
              (if List.mem cname mdef.P.l_coords    (* connected? *)
               then ((if crd!=0                         (* first? *)
                      then [(List.rev head) @ [crd-1] @ tail]
                      else [])
                     @ (if crd!=lim-1                   (* last?  *)
                        then [(List.rev head) @ [crd+1] @ tail]
                        else [])
                     @ neighs)
               else neighs)
        | _ -> failwith "lattice/neighbors: lengths differ"
    in loop [] crds limits pdef.P.p_coords []

  and noise noises acrds bcrds = 
    let noisetab = List.combine mdef.P.l_coords noises in
    let rec loop acrds bcrds cdefs =
      match (acrds, bcrds, cdefs) with
        | (a::acrds, b::bcrds, { P.c_name = cname}::cdefs) ->
            if (a != b) then List.assoc cname noisetab else loop acrds bcrds cdefs
        | _ -> failwith "lattice/noise called on unconnected nodes"
    in loop acrds bcrds pdef.P.p_coords

  and update ftr crds bel =
    modify matrix crds ftr bel (fun _ bel -> bel) in

  (* a separate field for each feature *)
  let fldtab =
    List.map (fun { P.ld_featr = feature; P.ld_noises = noises } -> 
                ((try
                    List.assoc feature ftrtab
                  with
                      Not_found -> raise (P.CheckError (P.Undecl_featr feature))),
                 Markov.make nodes neighbors (noise noises)))
      mdef.P.l_depnds in

  (* notify the right field *)
  let notify crds ftr upd =
    (List.assoc ftr fldtab).Markov.notify crds upd

  (* propagate in all fields *)
  and propagate () =
    let centers = (* modification centers, for more eficient propagation *)
      forall matrix List.concat (fun c -> if c.touched then [c.point] else [])
    in List.iter (fun (ftr, field) -> field.Markov.propagate (update ftr) centers) fldtab

  (* send prior beliefs as evidences in every node *)
  and send_priors () =
    forall matrix (fun _ -> ()) 
      (fun c ->
         List.iter (fun { P.ld_featr = feature; P.ld_prior = prior } ->
                      ( c.touched <- true
                      ; (List.assoc (List.assoc feature ftrtab) fldtab)
                        .Markov.notify c.point prior ))
           mdef.P.l_depnds)

  in ( send_priors ()
     ; propagate ()
     ; { n_matrix =  matrix;
         n_notify = notify;
         n_propagate = propagate} )

let make pdef =
  make_model
    (match pdef.P.p_model with
         P.Lattice mdef -> (lattice pdef mdef)
       | P.BIF _ -> failwith "make: model BIF not yet supported")

;;
  
(* tests *)
    
let check x y msg = if x<>y then failwith ("model: " ^ msg) in
let pdef = { P.null_model with 
               P.p_coords = [{ P.c_name = "x";
                               P.c_range = [0.0; 1.0]};
                             { P.c_name = "y";
                               P.c_range = [0.0; 2.0]}];
               P.p_featrs = ["a"; "b"];
               P.p_model = P.Lattice { P.l_coords= [];
                                       P.l_depnds= [{ P.ld_featr = "a";
                                                      P.ld_prior = Ndist.make (-.1.0) 1.0;
                                                      P.ld_noises = [] };
                                                    { P.ld_featr = "b";
                                                      P.ld_prior = Ndist.make 0.0 2.0;
                                                      P.ld_noises = [] }] } } in
let l = make pdef in
  ( check (beliefs_differ (0.0, 1.0) (1.0, 1.0)) true "means differ"
  ; check (beliefs_differ (1.0, 1.0) (1.0, 2.0)) true "variance differ"
  ; check (beliefs_differ Ndist.unknown Ndist.unknown) true "unkown belief differs"
  ; check (beliefs_differ (1.0, 2.0) (1.0, 2.0)) false "beliefs equal"
  ; check (l.fetch [Some 0;Some 1] 1) [(0.0, 2.0)] "fetch cell"
  ; check (l.fetch [Some 0;None] 0) [(-.1.0, 1.0); (-.1.0, 1.0)] "fetch row"
  ; check (l.fetch [None;Some 1] 1) [(0.0, 2.0);(0.0, 2.0)] "fetch column" 
  ; check (l.update [0; 0] 1 (2.0, 2.0) false; l.fetch [Some 0; Some 0] 1) [(1.0, 1.0)] "update" 
  ; check (l.modified [None;Some 0]) true "modified column"
  ; check (l.modified [Some 1;Some 1]) false "modified cell"
  ; check (l.modified [None; None]) true "modified any"
  ; check (l.rollback (); l.fetch [Some 0; Some 0] 1) [(0.0, 2.0)] "rollback"
  ; check (l.modified [None; None]) false "modified none" )
                     
