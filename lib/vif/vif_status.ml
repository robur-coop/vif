type informational = [ `Continue | `Switching_protocols ]

type successful =
  [ `OK
  | `Created
  | `Accepted
  | `Non_authoritative_information
  | `No_content
  | `Reset_content
  | `Partial_content ]

type redirection =
  [ `Multiple_choices
  | `Moved_permanently
  | `Found
  | `See_other
  | `Not_modified
  | `Temporary_redirect
  | `Use_proxy ]
(* | `Permanent_redirect ] *)

type client_error =
  [ `Bad_request
  | `Unauthorized
  | `Payment_required
  | `Forbidden
  | `Not_found
  | `Method_not_allowed
  | `Not_acceptable
  | `Proxy_authentication_required
  | `Request_timeout
  | `Conflict
  | `Gone
  | `Length_required
  | `Precondition_failed
  | `Payload_too_large
  | `Uri_too_long
  | `Unsupported_media_type
  | `Range_not_satisfiable
  | `Expectation_failed
  | `Misdirected_request
  | (* | `Too_early *)
    `Upgrade_required
  | `Precondition_required
  | `Too_many_requests
  | `Request_header_fields_too_large
  | `Enhance_your_calm
  | `I_m_a_teapot ]
(* | `Unavailable_for_legal_reasons ] *)

type server_error =
  [ `Internal_server_error
  | `Not_implemented
  | `Bad_gateway
  | `Service_unavailable
  | `Gateway_timeout
  | `Http_version_not_supported
  | `Network_authentication_required ]

type standard =
  [ informational | successful | redirection | client_error | server_error ]

type t = [ standard | `Code of int ]
