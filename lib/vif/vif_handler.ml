type ('socket, 'c, 'value) t =
     ('socket, 'c, string) Vif_request.t
  -> string
  -> Vif_server.t
  -> 'value
  -> (Vif_response.empty, Vif_response.sent, unit) Vif_response.t option
