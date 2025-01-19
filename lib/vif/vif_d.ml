type t = ..

module Device = struct
  type nonrec 'a t = {
      name: string
    ; initialize: t -> t * 'a
    ; finally: 'a -> unit
  }

  let make ~initialize ~finally name = { name; initialize; finally }
end

module Hmap = Hmap.Make (Device)

type t += Devices : Hmap.t -> t
(* NOTE(dinosaure): or module-rec? *)

type 'a arg =
  | Value : 'a Hmap.key -> 'a arg
  | Const : 'a -> 'a arg
  | Map : ('f, 'a) args * 'f -> 'a arg

and ('fu, 'return) args =
  | [] : ('r, 'r) args
  | ( :: ) : 'a arg * ('f, 'a -> 'r) args -> ('f, 'r) args

and 'a device = 'a Hmap.key

let map args fn = Map (args, fn)
let const value = Const value

let rec ctor : type a. t -> a arg -> t * a =
 fun devices -> function
  | Const v -> (devices, v)
  | Map (lst, fn) -> keval_args devices (fun devices x -> (devices, x)) lst fn
  | Value k -> (
      let[@warning "-8"] (Devices m) = devices in
      match Hmap.find k m with
      | None ->
          let devices, device = (Hmap.Key.info k).Device.initialize devices in
          let[@warning "-8"] (Devices devices) = devices in
          let devices = Hmap.add k device devices in
          (Devices devices, device)
      | Some device -> (devices, device))

and keval_args : type f x r. t -> (t -> x -> r) -> (f, x) args -> f -> r =
 fun devices k -> function
  | [] -> k devices
  | x :: r ->
      let devices, v = ctor devices x in
      let k devices fn = k devices (fn v) in
      keval_args devices k r

let device : type r.
    name:string -> finally:(r -> unit) -> ('f, r) args -> 'f -> r arg * r device
    =
 fun ~name ~finally args fn ->
  let initialize devices =
    let k devices v = (devices, v) in
    keval_args devices k args fn
  in
  let device = Device.make ~initialize ~finally name in
  let key = Hmap.Key.create device in
  (Value key, key)
