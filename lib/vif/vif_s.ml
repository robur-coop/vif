type t = { reqd: reqd; socket: socket; devices: Vif_d.Hmap.t }
and reqd = Httpcats.Server.reqd
and socket = [ `Tcp of Miou_unix.file_descr | `Tls of Tls_miou_unix.t ]

let reqd { reqd; _ } = reqd

let device : type a. ('value, a) Vif_d.device -> t -> a =
 fun (Vif_d.Device (_, _, k)) { devices; _ } ->
  match Vif_d.Hmap.find k devices with
  | Some value -> value
  | None ->
      Fmt.failwith "Device %s not found"
        (Vif_d.Hmap.Key.info k).Vif_d.Device.name
