#require "vif" ;;

exception Foo

let () = Printexc.register_printer @@ function
  | Foo -> Some "Foo"
  | _ -> None
;;

let default req target server () = raise Foo ;;

let () =
  Miou_unix.run @@ fun () ->
  Vif.run ~default [] ()
;;
