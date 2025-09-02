let exists { Vif_request.request; _ } key =
  let queries = Vif_request0.queries request in
  List.mem_assoc key queries

let get { Vif_request.request; _ } key =
  let queries = Vif_request0.queries request in
  match List.assoc_opt key queries with None -> [] | Some values -> values

let all { Vif_request.request; _ } = Vif_request0.queries request
