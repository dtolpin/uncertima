(* $Id: error.mli 985 2010-06-28 17:28:30Z tolpin $ *)

(* Error reporting in command-line utilities *)

(* problem definition error *)
val definition_error: Pdefinition.error -> Lexing.position option -> unit

(* object data error *)
val data_error: Object.error -> unit

(* general error *)
val error: string -> unit
