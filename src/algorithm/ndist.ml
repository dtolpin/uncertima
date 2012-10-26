(* $Id: ndist.ml 985 2010-06-28 17:28:30Z tolpin $ *)

type t = float*float

let make m v = (m,v)

let mean (m,_) = m
let variance (_,v) = v

let unknown = make 0.0 infinity
let known (_,v) = v <> infinity

let pi = acos (-1.0)

(* density *)
let pdf (m,v) x = let d=x-.m in exp(-.d*.d/.2.0/.v)/.sqrt(2.0*.pi*.v)

(* cumulative *)
let cdf (m,v) x =  (* Taylor approximation *)
  let scdf x =
    if x < -.8.0 then 0.0   (* rule out negative *) 
    else if x > 8.0 then 1.0 (* rule out greater than 1 *)
    else let rec loop term i sum =
      let nextsum = sum+.term
      in if abs_float (nextsum-.sum) > 0.0
        then loop (term*.x*.x/.i) (i+.2.0) nextsum
        else 0.5+.sum*.exp(-.x*.x*.0.5)/.sqrt(2.0*.pi)
    in loop x 3.0 0.0
  in scdf ((x-.m)/.sqrt v)

let saved: float option ref = ref None (* every other value is saved
                                          and returned immediately *)
let sample (m,v) =
  if v = 0.0 then m else
    let scale = let sd = sqrt(v) in fun x -> m+.x*.sd in
      match !saved with
          None ->
            let (* Box-Muller with rejection sampling *)
                u () = 2.0 *. (Random.float 1.0) -. 1.0 in
            let rec generate () = 
              let u1 = u () in
              let u2 = u () in
              let w = u1*.u1+.u2*.u2  in
                if w >= 1.0 then
                  generate ()
                else
                  let w = sqrt (-2.0*.log w/.w) in
                  let v1 = u1*.w in
                  let v2 = u2*.w in
                    (saved := Some v2; v1)
            in scale (generate ())
        | Some v2 -> (saved := None; scale v2)
