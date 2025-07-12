type t = { devices: Vif_device.Hmap.t; cookie_key: Mirage_crypto.AES.GCM.key }

let device : type a. ('value, a) Vif_device.device -> t -> a =
 fun (Vif_device.Device (_, _, k)) { devices; _ } ->
  match Vif_device.Hmap.find k devices with
  | Some value -> value
  | None ->
      Fmt.failwith "Device %s not found"
        (Vif_device.Hmap.Key.info k).Vif_device.Device.name

let cookie_key { cookie_key; _ } = cookie_key
