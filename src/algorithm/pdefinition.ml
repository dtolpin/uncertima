(* $Id: pdefinition.ml 985 2010-06-28 17:28:30Z tolpin $ *)

(* parsed problem definition datatypes *)

(* expression is computed in an environment where coordinates
   and sometimes features are defined, depending on the context *)

type section = string*float (* coordinate is equal to a constant *)

type expression =
    ExCon of float                         (* constant *)
  | ExVal of string                        (* a *)
  | ExDif of string                        (* @a *)
  | ExAdd of expression*expression         (* a+b *)
  | ExSub of expression*expression         (* a-b *)
  | ExMul of expression*expression         (* a*b *)
  | ExDiv of expression*expression         (* a/b *)
  | ExMod of expression*expression         (* a % b = int(a) mod int(b) *)
  | ExPow of expression*expression         (* a^b *)
  | ExNeg of expression                    (* -a *)
  | ExApp of string*expression list        (* e.g. max(a,b) *)
  | ExSec of string*section list           (* a[x=1, y=2] *)

(* coordinate in the observation space *)

type coordinate = { c_name: string;         (* name *)
                    c_range: float list }   (* values *)

type feature = string (* name *)

type observation = { o_featrs: string list;     (* features to observe *)
                     o_accurs: float list;      (* observation accuracies - \sigma^2 *)
                     o_cost: expression }       (* observation cost *)

type binding = { b_name: string;         (* name of variable *)
                 b_expr: expression }    (* expression to compute the value *)

type merit = { m_name: string;              (* name of merit variable *) 
               m_coords: string list;       (* coordinates to optimize over *)
               m_expr: expression; }        (* expression to compute the value *)

(* Lattice dependency model *)
(* dependency of a feature along each of the axes in the grid *)

type dependency = { ld_featr: string;         (* feature name *)
                    ld_prior: Ndist.t;        (* prior belief on node value *)
                    ld_noises: float list }   (* noise variances along each axis *)

type lattice_model = { l_coords: string list;        (* coordinates with dependencies *)
                       l_depnds: dependency list }   (* dependencies along coordinates *)
    
(* a model is defined either inline or in a separate file *)

type model = 
    Lattice of lattice_model (* independent or lattice model, specified inline *)
  | BIF of string            (* bayesian gaussian model in extended BIF *)

(* observations are indexed explicitly; and the external indices are used
   as well as internal references to observations, because it is easier
   to debug this way *)

type t = { p_name: string;                   (* problem name *)
           p_coords: coordinate list;        (* observation space coordinates *)
           p_cost: expression;
           p_featrs: feature list;           (* features to observe *)
           p_obsrvs: (int*observation) list; (* available observation actions *)
           p_bindgs: binding list;           (* auxiliary bindings *)
           p_find: merit;                    (* optimization function *)
           p_model: model }                  (* dependency model *)
    
(* error reporting during and after parsing *)

type error =
  | Illegal_char of char      (* error during scanning *)
  | Syntax_error              (* error during parsing *)
  | Undecl_coord of string    (* undeclared coordinate *)
  | Undefn_coord of string    (* undefined coordinate *)
  | Undecl_featr of string    (* undeclared feature *)
  | Unobsv_featr of string    (* undefined feature *)
  | Arities_differ            (* tuples must be of the same size *)
  | Unbound_name of string    (* unbound name in expression *)
  | Invalid_sect of string    (* invalid section of variable *)
  | Wrong_funtyp of string    (* wrong function type *)
  | Unavail_func of string    (* unavailable function *)
  | Wrong_contxt              (* wrong subexpression context *)
      
exception ParseError of error*Lexing.position
exception CheckError of error

(* null model for debugging *)
let null_model = { p_name = ""; p_coords = []; p_cost = ExCon 0.0;
                   p_featrs = []; p_obsrvs = []; p_bindgs = []; 
                   p_find = { m_name = ""; m_coords = []; m_expr = ExCon  0.0 };
                   p_model = Lattice { l_coords=[]; l_depnds = [] } } 
