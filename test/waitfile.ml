let () =
  match Sys.argv with
  | [| _; filename |] ->
      while Sys.file_exists filename = false do
        Unix.sleepf 0.1
      done
  | _ -> Format.printf "%s <filename>\n%!" Sys.executable_name
