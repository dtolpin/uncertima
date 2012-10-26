/* $Id: pparser.mly 607 2009-07-04 14:04:38Z tolpin $ */

/* parser for problem definition */
/* extensive comments on variables in problem.pli

   * productions with name in plural (names, values etc)
     return inversed list
   * productions with name ending in tuple return
     list in proper order
   * arithmetics in expressions on floats
*/

%{

open Pdefinition
open Lexing

let make_range first step last = 
  let step = if (last-.first>0.0) = (step>0.0) then step else -.step in 
  let rec add_point (points, cur) =
     let next = cur +. step in
     if cur <= last && next > last || cur >= last && next < last
     then List.rev points
     else add_point (next::points, next)
   in add_point ([first], first)

exception Wrapped_error of error 

(* set difference on lists *)
let setdiff a b = List.filter (fun n -> not (List.mem n b)) a

(* sanity checks: *)
(* declared and defined are the same, otherwise report a non-matching name *)
let check_names decld defnd edecl edefn =
  let undecl = setdiff defnd decld in
  let undefn = setdiff decld defnd in
  match undecl with
    n::_ -> raise (Wrapped_error (edecl n))
  | _ ->
      match undefn with
        n::_ -> raise (Wrapped_error (edefn n))
      | _ -> ()

(* make lattice, checking that the tuples are of the same arity *)
let make_lattice coords depnds =
  ( List.iter
      (fun dep ->
         if List.length dep.ld_noises <> List.length coords
         then raise (Wrapped_error Arities_differ))
      depnds
  ; Lattice {l_coords = coords; l_depnds = depnds} )

%}

/* tokens */
%token EOF 

%token <int> INT
%token <float> FLOAT
%token <string> STRING
%token <string> IDENT

/* keywords */
%token PROBLEM SPACE OBSERVE FIND MAX MIN MODEL INDEP LATTICE BIF GDL INFINITY

/* syntax */
%token LPAR RPAR LCUR RCUR LSQU RSQU COMMA SEMIC RARR AT DOTS
%token LT GT LE GE NE EQ NE PLUS MINUS STAR SLASH PERC POWER

/* precedence, binding */
%nonassoc LT GT LE NE EQ NE
%left PLUS MINUS
%left STAR SLASH PERC
%right POWER
%nonassoc UMINUS
%nonassoc APPLIC

%start problem
%type <Pdefinition.t> problem

%% 

problem: PROBLEM name LCUR space observe find model RCUR
        { let (coords, cost) = $4 in
          let (featrs, obsrvs) = $5 in
          let (bindgs, find) = $6 in
            { p_name = $2;
              p_coords = coords;
              p_cost = cost;
              p_featrs = featrs;
              p_obsrvs = obsrvs;
              p_bindgs = bindgs;
              p_find = find;
              p_model = $7 } } ;

space: SPACE nametuple optcost LCUR coordinates RCUR
        { check_names $2 (List.map (fun c -> c.c_name) $5)
            (fun undecl -> Undecl_coord undecl)
            (fun undefn -> Undefn_coord undefn);
          (List.rev $5, $3) } ;

optcost : SLASH expr { $2 } 
    | { ExCon 0.0 }

coordinates: { [] }
    | coordinates coordinate { $2::$1 } ;
coordinate : name EQ range optcost SEMIC
        { { c_name = $1;
            c_range = $3 } } ;

range: value {[$1]}          /* brackets can be omitted with a single value */
    | LSQU rangegen RSQU { $2 }
rangegen: value {[$1]}
    | value untilvalue { make_range $1 1.0 $2 }
    | value COMMA value morevalues { $4 $1 $3 }
untilvalue : DOTS value { $2 } ;
morevalues : { fun first last -> [first; last] }
    | untilvalue { fun first second -> make_range first (second -. first) $1  }
    | COMMA somevalues { fun first second -> first::second::List.rev $2 }

observe: OBSERVE nametuple LCUR observations RCUR
        { check_names $2 (List.fold_left (@) []
                            (List.map (fun (i,o) -> o.o_featrs) $4))
            (fun undecl -> Undecl_featr undecl)
            (fun unobsv -> Unobsv_featr unobsv);
         ($2, List.rev $4) } ;

observations: { [] }
    | observations observation { $2::$1 } ;
observation: INT RARR nametuple EQ valuetuple SLASH expr SEMIC
        { if (List.length $3) != (List.length $5)
          then raise (Wrapped_error Arities_differ)
        ; ($1, { o_featrs = $3;
                 o_accurs = $5;
                 o_cost = $7 }) } ;
  
find: FIND optimum { $2 } ;
optimum: MIN merit { $2 (fun e  -> ExNeg e) }
    | MAX merit { $2 (fun e -> e) } ;
merit: name nametuple EQ expr bindinglist
        { fun f -> ($5, (* bindings *)
                    { m_name = $1;
                      m_coords = $2; 
                      m_expr = f $4 }) } ;

bindinglist: SEMIC { [] }
    | LCUR bindings RCUR { List.rev $2 } ;
bindings: { [] }
    | bindings binding { $2::$1 } ;
binding: name EQ expr SEMIC
        { {b_name = $1;
           b_expr = $3} } ;

model: { Lattice { l_coords = []; l_depnds = []; }}
    | MODEL depmodel  { $2 } ;

depmodel: BIF STRING { BIF $2 } 
    | LATTICE nametuple LCUR dependencies RCUR 
        { make_lattice $2 (List.rev $4) }
    | INDEP LCUR priors RCUR /* internally indep is lattice with no dependencies */
        { make_lattice [] (List.rev $3) } ;

dependencies: { [] }
    | dependencies dependency { $2::$1 } ;
dependency: name EQ dist SLASH valuetuple SEMIC 
        { {ld_featr = $1;
           ld_prior = $3;
           ld_noises = $5} } ;

priors: { [] }
    | priors prior { $2::$1 } ;
prior: name EQ dist
        { {ld_featr = $1;
           ld_prior = $3;
           ld_noises = []} }

dist: LPAR value COMMA value RPAR { Ndist.make $2 $4 } ;

nametuple: name { [ $1 ] }
    | LPAR names RPAR { List.rev $2 } ;

names: { [] }
    | somenames { $1 }
somenames: name { [$1] }
    | somenames COMMA name { $3::$1 } ;
name: IDENT { $1 } ;

valuetuple: value { [ $1 ] }
    | LPAR values RPAR { List.rev $2 } ;

values: { [] }
    | somevalues { $1 }
somevalues: value { [$1] }
    | somevalues COMMA value { $3::$1 } ;

value: number { $1 }
    | MINUS number { -.$2 } ;

expr: number { ExCon $1 }
    | name { ExVal $1 }
    | AT name { ExDif $2 } 
    | LPAR expr RPAR { $2 }
    | expr PLUS expr { ExAdd ($1, $3) }
    | expr MINUS expr { ExSub ($1, $3) }
    | expr STAR expr { ExMul ($1, $3) }
    | expr SLASH expr { ExDiv ($1, $3) }
    | expr PERC expr  { ExMod ($1, $3) }
    | expr POWER expr { ExPow ($1, $3) }
    | MINUS expr %prec UMINUS
        { ExNeg $2 }
    | name LPAR exprs RPAR %prec APPLIC
        { ExApp ($1, List.rev $3) }
    | name LSQU sections RSQU 
        { ExSec ($1, List.rev $3) } ;

sections: { [] } 
    | sections section { $2::$1 } ;
section: name EQ value { ($1,$3) } ;

exprs : { [] }
    | exprs expr { $2::$1 } ;

number: INT { float $1 }
    | FLOAT { $1 } 
    | INFINITY { infinity } ;

%%

(* wrap the parser into syntax error reporting *)
let problem token lexbuf =
  try
    problem token lexbuf
  with
    Parse_error ->
      raise (ParseError (Syntax_error, lexbuf.lex_start_p))
  | Wrapped_error perror -> 
      raise (ParseError (perror, lexbuf.lex_start_p))
