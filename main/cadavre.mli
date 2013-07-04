(*********************************)
(*                               *)
(*         Cadavre.mli           *)
(*                               *)
(*********************************)

(** Inspiré du script d'A. Loyer, dit Drébon,
le cadavre permet d'animer des news avec des petites phrases personnalisables.*)

type data = Data.t
(* The data that will be marshalled in Config.data *)

type id = Nntp.id

val add_time : float * Unix.tm -> unit
val add_subject : id:id -> string -> int -> unit
val add_verb : id:id -> string array array -> unit
val add_comp : id:id -> string -> unit
val add_date : id:id -> string -> int -> unit
val add_place : id:id -> string -> unit
(* update the data *)

val save_data : unit -> unit
(* stores the data in a marshalled structure *)

val send_help : Nntp.id -> unit
(* answer to the help question *)

val last_time : unit -> float * Unix.tm
val last_seen_number : unit -> Nntp.number
val save_last_number : Nntp.number -> unit

val is_bored : unit -> bool
val post_sentence : unit -> unit
(* check if a new sentence should be posted, and post it *)
