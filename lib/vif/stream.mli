module Bqueue : sig
  type 'a t

  val create : int -> 'a t
  val put : 'a t -> 'a -> unit
  val get : 'a t -> 'a option
  val close : 'a t -> unit
end

type 'a source =
  | Source : {
        init: unit -> 's
      ; pull: 's -> ('a * 's) option
      ; stop: 's -> unit
    }
      -> 'a source

module Source : sig
  val file : ?offset:int -> string -> string source
  val dispose : 'a source -> unit
  val ppf : (Format.formatter -> unit) -> string source
end

type ('a, 'r) sink =
  | Sink : {
        init: unit -> 's
      ; push: 's -> 'a -> 's
      ; full: 's -> bool
      ; stop: 's -> 'r
    }
      -> ('a, 'r) sink

module Sink : sig
  val json : unit -> (string, (Json.t, [ `Msg of string ]) result) sink
  val into_bstream : 'a Multipart_form_miou.Bounded_stream.t -> ('a, unit) sink
  val string : (string, string) sink
end

type ('a, 'b) flow = { flow: 'r. ('b, 'r) sink -> ('a, 'r) sink } [@@unboxed]

module Flow : sig
  val identity : ('a, 'a) flow
  val compose : ('a, 'b) flow -> ('b, 'c) flow -> ('a, 'c) flow
  val ( >> ) : ('a, 'b) flow -> ('c, 'a) flow -> ('c, 'b) flow
  val ( << ) : ('a, 'b) flow -> ('b, 'c) flow -> ('a, 'c) flow

  val deflate :
       ?q:De.Queue.t
    -> ?w:De.Lz77.window
    -> ?level:int
    -> unit
    -> (string, string) flow
end

type 'a stream = { stream: 'r. ('a, 'r) sink -> 'r } [@@unboxed]

module Stream : sig
  val run :
       from:'a source
    -> via:('a, 'b) flow
    -> into:('b, 'c) sink
    -> 'c * 'a source option

  val into : ('a, 'b) sink -> 'a stream -> 'b
  val via : ('a, 'b) flow -> 'a stream -> 'b stream
  val from : 'a source -> 'a stream
  val of_bqueue : string Bqueue.t -> string stream
  val singleton : 'a -> 'a stream
end
