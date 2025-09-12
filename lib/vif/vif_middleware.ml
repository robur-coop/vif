module Key = struct
  type 'a t = { name: string }

  let make ~name = { name }
end

module Hmap = Hmap.Make (Key)

type ('socket, 'cfg, 'v) fn =
  'socket Vif_request0.t -> string -> Vif_server.t -> 'cfg -> 'v option

type ('socket, 'cfg, 'v) t =
  | Middleware : ('socket, 'cfg, 'v) fn * 'v Hmap.key -> ('socket, 'cfg, 'v) t

type ('socket, 'cfg) m =
  | [] : ('socket, 'cfg) m
  | ( :: ) : ('socket, 'cfg, 'a) t * ('socket, 'cfg) m -> ('socket, 'cfg) m

type ('socket, 'value, 'a, 'c) ctx = {
    server: Vif_server.t
  ; req0: 'socket Vif_request0.t
  ; target: string
  ; user's_value: 'value
}

let v : type v. name:string -> ('socket, 'cfg, v) fn -> ('socket, 'cfg, v) t =
 fun ~name fn ->
  let key = Hmap.Key.create (Key.make ~name) in
  Middleware (fn, key)

let rec run : type v.
    ('socket, v) m -> ('socket, v, 'a, 'c) ctx -> Hmap.t -> Hmap.t =
 fun lst ctx env ->
  match lst with
  | [] -> env
  | Middleware (fn, key) :: r -> begin
      match fn ctx.req0 ctx.target ctx.server ctx.user's_value with
      | Some value -> run r ctx (Hmap.add key value env)
      | None -> run r ctx env
      | exception _exn -> run r ctx env
    end
