type pool_info = { name : string; capacity : int; running : int; queue : int; utilization : float }

val parse_pool_list : string -> string list
(** Parse the output of "ocluster-admin show" to extract pool names *)

val parse_pool_details : string -> string -> pool_info
(** Parse the output of "ocluster-admin show <pool>" to extract pool details *)

val run_command : string -> string
(** Execute a shell command and return its output *)

val get_pools : unit -> string list
(** Get list of all available pools *)

val get_pool_info : string -> pool_info
(** Get detailed information about a specific pool *)
