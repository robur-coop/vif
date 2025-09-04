#require "vif";;

let field = "content-type"

let number req number _server () =
  let open Vif.Response.Syntax in
  let* () = Vif.Response.add ~field "text/plain; charset=utf-8" in
  let* () =
    Printf.ksprintf (Vif.Response.with_string req)
      "Hello World! The number is %u!\n" number
  in
  Vif.Response.respond `OK

let horse req _server () =
  let open Vif.Response.Syntax in
  let* () = Vif.Response.add ~field "text/plain; charset=utf-8" in
  let* () = Vif.Response.with_string req "The horse goes prrrrr!\n" in
  Vif.Response.respond `OK


let routes =
  let open Vif.Uri in
  let open Vif.Route in
  let bad_int = conv int_of_string string_of_int (string `Path) in
  (* The first route matches ["/42"] and triggers [number]. The first route
     also matches ["/horse"], but because [int_of_string "horse"] raises it
     does not trigger [number]. The route [horse] cannot match due to it being
     shadowed by [number]. This shows that you should be careful with what
     regular expressions you use when combining them with converters. *)
  [ get (rel /% bad_int /?? nil) --> number
  ; get (rel / "horse" /?? nil) --> horse ]

let () =
  Miou_unix.run @@ fun () ->
  Vif.run routes ()
;;
