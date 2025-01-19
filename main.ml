#require "miou.unix"

#require "mirage-crypto-rng-miou-unix"

#require "vif"

#require "digestif.c"

#require "base64"

let index server _req () =
  Vif.Response.with_string server `OK "Hello from an OCaml script!"

let test arg server _req () =
  Vif.Response.with_string server `OK (Fmt.str "%02x\n%!" arg)

let digest server req () =
  let ic = Vif.Request.to_stream req in
  let rec go ctx =
    match Vif.Stream.get ic with
    | Some str -> go (Digestif.SHA1.feed_string ctx str)
    | None -> Digestif.SHA1.get ctx
  in
  let hash = go Digestif.SHA1.empty in
  let hash = Digestif.SHA1.to_hex hash in
  Vif.Response.with_string server `OK hash

let random len server req () =
  let buf = Bytes.create 0x7ff in
  Vif.Response.with_stream server `OK @@ fun oc ->
  let rec go rem =
    Format.printf ">>> %d\n%!" rem;
    if rem > 0 then begin
      let len = Int.min rem (Bytes.length buf) in
      Mirage_crypto_rng.generate_into buf len;
      let str = Bytes.sub_string buf 0 len in
      let str = Base64.encode_exn str in
      Vif.Stream.put oc str;
      go (rem - len)
    end
  in
  go len

let routes =
  let open Vif.U in
  let open Vif.R in
  [
    (rel /?? nil) --> index; (rel / "random" /% Tyre.int /?? nil) --> random
  ; (rel / "test" /% Tyre.int /?? nil) --> test
  ; (rel / "digest" /?? nil) --> digest
  ]

let default target server req () =
  Vif.Response.with_string server `Not_found (Fmt.str "%S not found\n%!" target)

let my_device_as_arg, my_device =
  Vif.D.device ~name:"my-device" ~finally:ignore [] ()

let () =
  Miou_unix.run @@ fun () ->
  let sockaddr = Unix.(ADDR_INET (inet_addr_loopback, 8080)) in
  let cfg = Vif.config sockaddr in
  Vif.run ~cfg ~default ~devices:Vif.[ D.rng; my_device_as_arg ] routes ()
