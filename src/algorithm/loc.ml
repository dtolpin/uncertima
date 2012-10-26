(* $Id: loc.ml 985 2010-06-28 17:28:30Z tolpin $ *)

type point = int list
type section = int option list
type subspace = section

type obs = point
type opt = section

type act = int * obs

let specialize loc sect = 
  List.map2 (fun a b ->
               match a with
                   Some _ -> a
                 | _ -> b) loc sect

(* list 0 .. n-1 *)
let iota n =
  let rec loop i =
    if i=n then []
    else i::loop (i+1)
  in loop 0

let comprehend sect space =
  let rec loop sect space =
    match (sect,space) with
        ([], []) -> [[]]
      | (se::sect, sp::space) ->
          (match (se, sp) with                     (* distribute the head over the tail *)
               (Some _, _) -> List.map (fun cs -> se::cs)     (* coordinate is definite *)
             | (_, None) -> List.map (fun cs -> None::cs) (* coordinate is not expanded *)
             | (_, Some n) ->                                  (* expand the coordinate *)
                 fun l ->
                   (List.concat
                      (List.map
                         (fun cs -> (List.map (fun c -> (Some c::cs)) (iota n)))
                         l)))
            (loop sect space)
      | _ -> failwith "comprehend: lengths differ"
  in loop sect space

let pierce sects points = 
  let rec covers s p = 
    match (s,p) with
        ([],[]) -> true
      | (cs::s, cp::p) -> 
          (match cs with
               Some cs -> cs=cp && covers s p
             | _ -> covers s p)
      | _ -> failwith "pierce: lengths differ"
  in List.filter (fun s -> List.exists (covers s) points) sects

;;

(* tests *)

let check x y z = if (specialize x y) <> z then failwith "specialize" 
in ( check [None] [None] [None]
   ; check [Some 1] [None] [Some 1]
   ; check [None] [Some 1] [Some 1]
   ; check [Some 2] [Some 1] [Some 2]
   ; check [Some 1; None] [None; Some 2] [Some 1; Some 2] )

;;

let check x y = if (iota x) <> y then failwith "iota"
in ( check 0 [] 
   ; check 1 [0]
   ; check 3 [0;1;2] )

;;

let check x y z = if (comprehend x y) <> z then failwith "comprehend"
in ( check [] [] [[]]
   ; check [None] [None] [[None]]
   ; check [Some 1] [None] [[Some 1]]
   ; check [Some 1] [Some 2] [[Some 1]]
   ; check [None] [Some 2] [[Some 0]; [Some 1]]
   ; check [None; None] [Some 2; Some 2] 
     [[Some 0; Some 0]; [Some 1; Some 0]; [Some 0; Some 1]; [Some 1; Some 1]]
   ; check [None; Some 1] [Some 2; Some 2]
     [[Some 0; Some 1]; [Some 1; Some 1]] )

;;

let check x y z = if (pierce x y) <> z then failwith "pierce" 
in ( check [] [] []
   ; check [[None]] [[1]; [2]] [[None]]
   ; check [[Some 1]; [None]; [Some 2]] [[1]] [[Some 1]; [None]] )


let thesection = List.map (fun i -> Some i)


