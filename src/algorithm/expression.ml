(* $Id: expression.ml 985 2010-06-28 17:28:30Z tolpin $ *)

module P = Pdefinition

type environment =
    { env_coord: int -> float;
      env_cdiff: int -> float;
      env_bindg: int -> float;
      env_section: int -> int option list -> float list }

(* by default, raise exception on any request;
   can be used to create partial environments *)
 
let null_environment =
  let u () = failwith "null environment" in
  { env_coord = (fun _ -> u ());
    env_cdiff = (fun _ -> u ());
    env_bindg = (fun _ -> u ());
    env_section = (fun _ _ -> u ()) }

type t = environment -> float

(* the builtin functions belong to one of the following types: *)
type libfun = 
    Vec_fun of (float list -> float)
  | Sca_fun of (float -> float)
  | Sca2_fun of (float -> float -> float)

(* library of builtin functions *)      
let funtab = 
  let sum = List.fold_left (+.) 0.0 in
  let mean l = (sum l) /. (float (List.length l)) in 
  let var l = 
    let m = mean l in
    let n = float (List.length l) in
      (sum (List.map (fun x -> x*.x) l) -. n *. m *. m) /. (n -. 1.0) in
  let sd l = sqrt (var l) in
    [ (* built-in operators *)
      "~", Sca_fun (~-.);
      "+", Sca2_fun (+.);
      "-", Sca2_fun (-.);
      "*", Sca2_fun ( *.);
      "/", Sca2_fun (/.);
      "%", Sca2_fun mod_float;
      "^", Sca2_fun ( **);

      (* 1.0 when not zero, useful for movement costs *)
      "nz", Sca_fun (fun x -> if (abs_float x) >= min_float then 1.0 else 0.0);

      (* sign function -1.0:0.0:1.0 *)
      "sgn", Sca_fun (fun x ->
                        if x < -.min_float then -1.0
                        else if x < min_float then 0.0
                        else 1.0);

      (* rect function - *)
      "rct", Sca_fun (fun x ->
                        if x < -.0.5 then 0.0
                        else if x < 0.5 then 1.0
                        else 0.0);

      (* rounding and absolute value *)
      "ceil", Sca_fun ceil;
      "floor", Sca_fun floor;
      "abs", Sca_fun abs_float;

      (* elementary functions *)
      "sqrt", Sca_fun sqrt;
      "exp", Sca_fun exp;
      "log", Sca_fun log;
      "ln", Sca_fun log;
      "log10", Sca_fun log10;
      "cos", Sca_fun cos;
      "sin", Sca_fun sin;
      "tan", Sca_fun tan;
      "acos", Sca_fun acos;
      "asin", Sca_fun asin;
      "atan", Sca_fun atan;
      "atan2", Sca2_fun atan2;
      "cosh", Sca_fun cosh;
      "sinh", Sca_fun sinh;
      "tanh", Sca_fun tanh;

      (* vector functions *)
      "min", Vec_fun (List.fold_left (min) max_float);
      "max", Vec_fun (List.fold_left (max) (-.max_float));
      "sum", Vec_fun sum;
      "mean", Vec_fun mean;
      "var", Vec_fun var;
      "sd", Vec_fun sd ]

(* return index for the name in the name list *)
let namidx names =
    let nametab = Pops.enumerate names in
      fun name -> 
        try List.assoc name nametab
        with Not_found -> raise (P.CheckError (P.Unbound_name name))

(* an expression is compiled into a closure, such that variables
   are addressed by array indices and all references to functions
   are resolved  to their functional values *)

let compile pdef =

  let coordinates = pdef.P.p_coords
  and features = pdef.P.p_featrs
  and bindings = pdef.P.p_bindgs in

  (* functions that returns index for name *)
  let crdidx = namidx (List.map (fun c -> c.P.c_name) coordinates)
  and varidx = namidx features
  and bdgidx = namidx (List.map (fun b -> b.P.b_name) bindings) in

  (* (name*value) list -> Loc.section *)
  let secrds =
    let crdtab = Pops.tabulate coordinates in
      fun sections -> 
        (* first convert sections from (name*value) to (index*index) 
           to check the names *)
        let secidxs =
          List.map 
            (fun (name, value) ->
               let idx = crdidx name in (* the name is checked here *)
                 (idx, 
                  try List.assoc value (List.assoc name crdtab)
                  with Not_found -> raise (P.CheckError (P.Invalid_sect name))))
            sections in

          (* then map each coordinate to either Some index or None *)
          List.map 
            (fun (name, _) ->
               try
                 Some (List.assoc (crdidx name) secidxs)
               with Not_found -> None)
            crdtab in

  (* all variable values, no section *)
  let anycrds = secrds [] in

  let some_fun name = 
    try List.assoc name funtab 
    with Not_found -> raise (P.CheckError (P.Unavail_func name)) in

  (* unary and binary arithmetic operators are defined in the environment
     and retrieved by their names (unary '-' is '~') *)

  let unop name = 
    match some_fun name with
        Sca_fun f -> f
      | _ -> raise (P.CheckError (P.Wrong_funtyp name))

  and binop name =
    match some_fun name with
        Sca2_fun f -> f
      | _ -> raise (P.CheckError (P.Wrong_funtyp name)) in

  (* arithmetics on scalars is just application *)

  let rec sca_unop f a =
    let fa = sca_comp a in
      fun env -> f (fa env) 

  and sca_binop f a b = 
    let fa = sca_comp a in
    let fb = sca_comp b in
      fun env -> f (fa env) (fb env)

  (*  arithmetics on vectors is componentwise *)

  and vec_unop f a = 
    let fa = vec_comp a in
      fun env -> List.map f (fa env)

  and vec_binop f a b = 
    let fa = vec_comp a in
    let fb = vec_comp b in
      fun env -> List.map2 f (fa env) (fb env)

  (* the top-level context is always scalar, and can be switched by a call
     to an aggregate (vector->scalar) function *)

  and sca_comp edef =
    match edef with
        P.ExCon number -> (fun env -> number)
      | P.ExVal name -> 
          ( try                                       (* binding, if present *)     
              let idx = bdgidx name in
                fun env -> env.env_bindg idx
            with P.CheckError (P.Unbound_name _) ->
              try                                     (* feature, if present *)
                let idx = varidx name in
                  fun env -> List.hd (env.env_section idx anycrds)
              with P.CheckError (P.Unbound_name _) ->
                let idx = crdidx name in              (* coordinate *)
                  fun env -> env.env_coord idx )
      | P.ExDif name ->
          let idx = crdidx name in
            fun env -> env.env_cdiff idx
      | P.ExAdd (a,b) -> sca_binop (binop "+") a b
      | P.ExSub (a,b) -> sca_binop (binop "-") a b
      | P.ExMul (a,b) -> sca_binop (binop "*") a b
      | P.ExDiv (a,b) -> sca_binop (binop "/") a b
      | P.ExMod (a,b) -> sca_binop (binop "%") a b
      | P.ExPow (a,b) -> sca_binop (binop "^") a b
      | P.ExNeg a -> sca_unop (unop "~") a
      | P.ExApp (name,args) -> 
          ( match (some_fun name,args) with
                (Sca_fun f,[a]) -> sca_unop f a
              | (Sca2_fun f,[a;b]) -> sca_binop f a b
              | (Vec_fun f,[a]) ->
                  (let fa = vec_comp a in (fun env -> f (fa env)))
              | _  -> raise (P.CheckError (P.Wrong_funtyp name)) )
      | P.ExSec (name,sections) ->
            let idx = varidx name in
            let crds = secrds sections in
              fun env -> List.hd (env.env_section idx crds)

  (* the vector context is for arguments of vector-to-scalar functions (Vec_fun) *)

  and vec_comp edef =
    match edef with
        P.ExVal name ->
            let idx = varidx name in
              fun env -> env.env_section idx anycrds
      | P.ExAdd (a,b) -> vec_binop (binop "+") a b
      | P.ExSub (a,b) -> vec_binop (binop "-") a b
      | P.ExMul (a,b) -> vec_binop (binop "*") a b
      | P.ExDiv (a,b) -> vec_binop (binop "/") a b
      | P.ExMod (a,b) -> vec_binop (binop "%") a b
      | P.ExPow (a,b) -> vec_binop (binop "^") a b
      | P.ExNeg a -> vec_unop (unop "~") a
      | P.ExApp (name,args) -> 
          ( match (some_fun name,args) with
                (Sca_fun f,[a]) -> vec_unop f a
              | (Sca2_fun f,[a;b]) -> vec_binop f a b
              | _  -> raise (P.CheckError (P.Wrong_funtyp name)) )
      | P.ExSec (name,sections) ->
            let idx = varidx name in
            let crds = secrds sections in
              fun env -> env.env_section idx crds
      | _ -> raise (P.CheckError P.Wrong_contxt)
              
  in sca_comp

;;

(* tests *)

let e = { env_coord = (fun i -> [| 1.0; 2.0 |].(i));
          env_cdiff = (fun i -> [| 2.0; 3.0 |].(i));
          env_bindg = (fun i -> [| 0.0; 1.0 |].(i));
          env_section = (fun _ -> 
                           function 
                               [Some 0;None] -> [2.0; 4.0]
                             | [None;Some 1] -> [3.0; 5.0]
                             | [Some 1;Some 0] -> [0.0]
                             | [None;None] -> [-.1.0; 0.0; 1.0]
                             | _ -> failwith "empty section") } in
let pdef = { P.null_model with
               P.p_coords = [{ P.c_name = "x";
                               P.c_range = [-.1.0; 1.0; 3.0]};
                             { P.c_name = "y";
                               P.c_range = [-.1.0; 2.0; 5.0]}];
               P.p_featrs = ["a"; "b"];
               P.p_bindgs = [{ P.b_name = "p"; 
                               P.b_expr = P.ExCon infinity };
                             { P.b_name = "q";
                               P.b_expr = P.ExCon infinity }]; } in
let check x y msg = 
  if not (((compile pdef x) e)=y)
  then failwith ("expression: " ^ msg) in
  ( check (P.ExCon 0.0) 0.0 "con"
  ; check (P.ExVal "x") 1.0 "crd"
  ; check (P.ExDif "y") 3.0 "dif"
  ; check (P.ExVal "a") (-1.0) "ftr"
  ; check (P.ExAdd (P.ExVal "x", P.ExCon 4.0)) 5.0 "add"
  ; check (P.ExSub (P.ExVal "q", P.ExVal "x")) 0.0 "sub" 
  ; check (P.ExPow (P.ExCon 2.0, P.ExCon 3.0)) 8.0 "pow"
  ; check (P.ExApp ("cos", [P.ExCon 0.0])) 1.0 "app" 
  ; check (P.ExApp ("min", [P.ExVal "a"])) (-1.0) "min"
  ; check (P.ExApp ("max", [P.ExVal "b"])) 1.0 "max"
  ; check (P.ExApp ("mean", [P.ExVal "a"])) 0.0 "mean"
  ; check (P.ExApp ("mean", [P.ExSec ("a",[("y",2.0)])])) 4.0 "sec 1" 
  ; check (P.ExApp ("max", [P.ExSec ("b",[("x",1.0);("y",-1.0)])])) 0.0 "sec 2" )


  

             
  

