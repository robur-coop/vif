module Assoc : sig
  type t = (string * string list) list
end

module Path : sig
  type t = private string list

  val of_string : string -> (t, [> `Msg of string ]) result
  val of_string_exn : string -> t
  val pp : Format.formatter -> t -> unit
end

type t

val pp : Format.formatter -> t -> unit
val parser : string -> (t list, [> `Msg of string ]) result

val search :
     roots:string list
  -> ?predicates:string list
  -> Path.t
  -> ((string * Assoc.t) list, [> `Msg of string ]) result

val ancestors :
     roots:string list
  -> ?predicates:string list
  -> Path.t
  -> ((Path.t * string * Assoc.t) list, [> `Msg of string ]) result

val to_artifacts :
  (string * Assoc.t) list -> (Objinfo.t list, [> `Msg of string ]) result

val setup : Fpath.t list Cmdliner.Term.t

(**/**)

val to_dir_path : string -> string
