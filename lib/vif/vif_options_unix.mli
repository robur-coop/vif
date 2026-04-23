val reporter : Re.t option -> Format.formatter -> Logs.reporter
val config_from_globals : unit -> (Vif_config_unix.config, string) result
val setup_config : unit Cmdliner.Term.t
