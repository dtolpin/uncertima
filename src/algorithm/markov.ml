(* $Id: markov.ml 985 2010-06-28 17:28:30Z tolpin $ *)

let debug = Debug.info "markov"

type 'a t = { notify: 'a -> Ndist.t -> unit;
              propagate: ('a -> Ndist.t -> bool) -> 'a list -> unit }

type neighbor = { noise: float;
                  mutable msg: Ndist.t }

type 'a node = { idx: int; (* index of the node in the network *)
                 crd: 'a;  (* coordinates of the node in the problem space *)
                 mutable neighbors: (int*neighbor) list }

let evd_idx = -1  (* evidence index, not matching any node *)

let make crds neighbors noise =
  (* enumerate nodes *)
  let index =
    let crdtab = Pops.enumerate crds
    in fun crd -> List.assoc crd crdtab in

  (* make neighbor list for a node *)
  let make_neighbors crd = 
    List.map (* initialize all messages from neighbors as unknown *)
       (fun ncrd -> (index ncrd,
                     { noise = noise crd ncrd;
                       msg = Ndist.unknown }))
       (neighbors crd) in

  (* lattice is an array of nodes *)
  let lattice = Array.of_list
       (List.map 
          (fun crd
             -> { idx = index crd;
                  crd = crd; 
                  neighbors = (make_neighbors crd) })
          crds) in

  let notify crd msg = 
    let node = lattice.(index crd)
    in node.neighbors <- (evd_idx, {noise = infinity; msg = msg})::node.neighbors in

  (* inbound: new belief *)
  let receive_messages node = 
    let vn = 1. /. (List.fold_left (fun sum (_, { msg = (_, v) }) -> sum +. 1. /. v )
                      0.0 node.neighbors) in
    let mn = vn *. (List.fold_left (fun sum (_, { msg = (m, v) }) -> sum +. m /. v )
                      0.0 node.neighbors)
    in Ndist.make mn vn in

  (* outbound: messages to neighbors *)
  let send_messages node =
    let to_neighbor (nidx, neighbor) = 
      let v0 = 1. /. (List.fold_left (fun sum (i, { msg = (_, v) }) -> 
                                        if i <> nidx then sum +. 1. /. v else sum)
                        0.0 node.neighbors) in
      let vmsg = neighbor.noise +. v0 in
      let mmsg = v0 *. (List.fold_left (fun sum (i, { msg = (m, v) }) -> 
                                          if i <> nidx then sum +. m /. v else sum)
                          0.0 node.neighbors)
      in (List.assoc node.idx lattice.(nidx).neighbors).msg <- Ndist.make mmsg vmsg
    in List.iter (fun ((i, _) as i_n) -> if i <> evd_idx then to_neighbor i_n) node.neighbors in
    
  let propagate update centers =

    (* compute traverse order from the centers to neighbors recursively *)
    let order =
      List.map index
        (let rec loop hier front =
           match front with
               [] -> List.rev hier
             | _ ->
                 let hier = List.rev_append front hier in
                 let front = 
                   List.fold_left
                     (fun new_front f ->
                        if List.mem f new_front || List.mem f hier
                        then new_front
                        else f::new_front)
                     [] (List.concat (List.map neighbors front))
                 in loop hier front
         in loop [] centers) in
                 
    let _ = debug (Printf.sprintf "|order|=%d" (List.length order)) in

    (* propagate iteratively until convergence or iteration limit *)
    let rec propagate iter =
      if (List.fold_left
            (fun modified idx ->
               let node = lattice.(idx)
               in ( send_messages node
                  ; update node.crd (receive_messages node) || modified ))
            false order)
        && iter < !Conf.number_of_propagations then propagate (iter+1)
    in propagate 0

  in { notify = notify;
       propagate = propagate }

          
  
