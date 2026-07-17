type t = {
    informational: int Atomic.t
  ; successful: int Atomic.t
  ; redirection: int Atomic.t
  ; client_error: int Atomic.t
  ; server_error: int Atomic.t
}

let empty () =
  {
    informational= Atomic.make 0
  ; successful= Atomic.make 0
  ; redirection= Atomic.make 0
  ; client_error= Atomic.make 0
  ; server_error= Atomic.make 0
  }
