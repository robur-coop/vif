let failwithf fmt = Format.kasprintf failwith fmt
let strf fmt = Format.kasprintf Fun.id fmt

let rec wait_for_daemon ?(port = 9457) ?(deadline = Unix.gettimeofday () +. 10.)
    () =
  let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let finally () = Unix.close sock in
  let connected =
    Fun.protect ~finally @@ fun () ->
    match
      Unix.connect sock (Unix.ADDR_INET (Unix.inet_addr_loopback, port))
    with
    | () -> true
    | exception Unix.Unix_error _ -> false
  in
  if connected then ()
  else if Unix.gettimeofday () > deadline then
    failwithf "vif daemon did not start in time"
  else begin
    Unix.sleepf 0.1;
    wait_for_daemon ~port ~deadline ()
  end

let port = ref 9457
let spec = [ ("-p", Arg.Set_int port, "Port to listen") ]

let () =
  Arg.parse spec ignore (strf "%s [-p PORT]" Sys.executable_name);
  wait_for_daemon ~port:!port ()
