module Key = struct
  type 'a t = { name: string }

  let make ~name = { name }
end

module Hmap = Hmap.Make (Key)

type ('cfg, 'v) fn = Vif_request0.t -> string -> Vif_g.t -> 'cfg -> 'v option
type ('cfg, 'v) t = Middleware : ('cfg, 'v) fn * 'v Hmap.key -> ('cfg, 'v) t
type 'cfg m = [] : 'cfg m | ( :: ) : ('cfg, 'a) t * 'cfg m -> 'cfg m

type ('value, 'a, 'c) ctx = {
    server: Vif_g.t
  ; req0: Vif_request0.t
  ; target: string
  ; user's_value: 'value
}

let make : type v. name:string -> ('cfg, v) fn -> ('cfg, v) t =
 fun ~name fn ->
  let key = Hmap.Key.create (Key.make ~name) in
  Middleware (fn, key)

let rec run : type v. v m -> (v, 'a, 'c) ctx -> Hmap.t -> Hmap.t =
 fun lst ctx env ->
  match lst with
  | [] -> env
  | Middleware (fn, key) :: r -> begin
      match fn ctx.req0 ctx.target ctx.server ctx.user's_value with
      | Some value -> run r ctx (Hmap.add key value env)
      | None -> run r ctx env
      | exception _exn -> run r ctx env
    end
