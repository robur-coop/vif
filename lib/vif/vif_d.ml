type t = ..

module Device = struct
  type nonrec 'a t = { name: string; finally: 'a -> unit }

  let make ~name finally = { name; finally }
end

module Hmap = Hmap.Make (Device)

let failwithf fmt = Format.kasprintf failwith fmt

type t += Devices : Hmap.t -> t

let empty = Devices Hmap.empty

type ('value, 'a) arg =
  | Value : 'a Hmap.key -> ('value, 'a) arg
  | Const : 'a -> ('value, 'a) arg
  | Map : ('value, 'fn, 'r) args * 'fn -> ('value, 'r) arg

and ('value, 'fn, 'r) args =
  | [] : ('value, 'value -> 'r, 'r) args
  | ( :: ) :
      ('value, 'a) arg * ('value, 'fn, 'r) args
      -> ('value, 'a -> 'fn, 'r) args

let rec arg : type a v. t -> v -> (v, a) arg -> t * a =
 fun devices user's_value -> function
  | Const v -> (devices, v)
  | Value k ->
      let[@warning "-8"] (Devices m) = devices in
      begin
        match Hmap.find k m with
        | None -> failwithf "Device %s not found" (Hmap.Key.info k).name
        | Some device -> (devices, device)
      end
  | Map (args, fn) ->
      let v = ref None in
      let k fn devices =
        v := Some devices;
        fn user's_value
      in
      let value = keval_args devices user's_value k args fn in
      (Option.get !v, value)

and keval_args : type f r v.
    t -> v -> ((v -> r) -> t -> r) -> (v, f, r) args -> f -> r =
 fun devices user's_value k -> function
  | [] -> fun fn -> k fn devices
  | x :: r ->
      let devices, v = arg devices user's_value x in
      fun fn ->
        let k fn devices = k fn devices in
        (keval_args devices user's_value k r) (fn v)

type ('v, 'r) device =
  | Device : ('v, 'f, 'r) args * 'f * 'r Hmap.key -> ('v, 'r) device

let const v = Const v
let value (Device (_, _, key)) = Value key
let map args fn = Map (args, fn)

let device : type v f r.
    name:string -> finally:(r -> unit) -> (v, f, r) args -> f -> (v, r) device =
 fun ~name ~finally args fn ->
  let key : r Hmap.key = Hmap.Key.create { name; finally } in
  Device (args, fn, key)

let run : type v. t -> v -> (v, 'r) device -> t =
 fun devices user's_value (Device (args, fn, key)) ->
  let v = ref None in
  let k fn devices =
    v := Some devices;
    fn user's_value
  in
  let x = keval_args devices user's_value k args fn in
  let[@warning "-8"] (Devices t) = Option.get !v in
  Devices (Hmap.add key x t)
