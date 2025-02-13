type t = { devices: Vif_d.Hmap.t; cookie_key: Mirage_crypto.AES.GCM.key }

let device : type a. ('value, a) Vif_d.device -> t -> a =
 fun (Vif_d.Device (_, _, k)) { devices; _ } ->
  match Vif_d.Hmap.find k devices with
  | Some value -> value
  | None ->
      Fmt.failwith "Device %s not found"
        (Vif_d.Hmap.Key.info k).Vif_d.Device.name

let cookie_key { cookie_key; _ } = cookie_key
