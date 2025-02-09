type t = V1 of H1.Request.t | V2 of H2.Request.t

let of_reqd = function
  | `V1 reqd -> V1 (H1.Reqd.request reqd)
  | `V2 reqd -> V2 (H2.Reqd.request reqd)

let headers = function
  | V1 req -> H1.Headers.to_list req.H1.Request.headers
  | V2 req -> H2.Headers.to_list req.H2.Request.headers

let meth = function
  | V1 req -> req.H1.Request.meth
  | V2 req -> req.H2.Request.meth

let target = function
  | V1 req -> req.H1.Request.target
  | V2 req -> req.H2.Request.target
