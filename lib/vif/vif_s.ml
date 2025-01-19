type t = { reqd: reqd; socket: socket; devices: Vif_d.Hmap.t }
and reqd = Httpcats.Server.reqd
and socket = [ `Tcp of Miou_unix.file_descr | `Tls of Tls_miou_unix.t ]
and 'a device = 'a Vif_d.device

let reqd { reqd; _ } = reqd

let device : type a. a Vif_d.device -> t -> a =
 fun k { devices; _ } ->
  match Vif_d.Hmap.find k devices with
  | Some value -> value
  | None -> failwith "Device not found"
