open! Dune_engine
open Stdune

(** The default environment in which to run commands. The environemnt of
    individual build contexts augment this environment. *)
val env : unit -> Env.t

(** Initialises this module. *)
val init : capture_outputs:bool -> unit
