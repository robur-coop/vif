type metrics = {
  informational : int ;
  successful : int ;
  redirection : int ;
  client_error : int ;
  server_error : int ;
}

let empty_metrics = {
  informational = 0 ;
  successful = 0 ;
  redirection = 0 ;
  client_error = 0 ;
  server_error = 0 ;
}

type t = {
  devices: Vif_device.Hmap.t;
  cookie_key: Mirage_crypto.AES.GCM.key;
  mutable metrics: metrics;
}

let metrics t = t.metrics

let device : type a. ('value, a) Vif_device.device -> t -> a =
 fun (Vif_device.Device (_, _, k)) { devices; _ } ->
  match Vif_device.Hmap.find k devices with
  | Some value -> value
  | None ->
      Fmt.failwith "Device %s not found"
        (Vif_device.Hmap.Key.info k).Vif_device.Device.name

let cookie_key { cookie_key; _ } = cookie_key
