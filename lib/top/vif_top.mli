type cfg

type error =
  [ `Syntax of Lexing.position * Location.report
  | `Report of Location.report option ]

val pp_error : ?file:string -> error Fmt.t
val config : stdlib:Fpath.t -> string list -> cfg
val eval : filename:string -> cfg -> lines:string list -> (unit, error) result
