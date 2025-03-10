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
  val list : 'a list -> 'a source
  val dispose : 'a source -> unit
  val with_formatter : (Format.formatter -> unit) -> string source
  val with_task : limit:int -> ('a Bqueue.t -> unit) -> 'a source
  val of_bqueue : 'a Bqueue.t -> 'a source
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
  val string : (string, string) sink
  val list : ('a, 'a list) sink
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

  val gzip :
       ?q:De.Queue.t
    -> ?w:De.Lz77.window
    -> ?level:int
    -> unit
    -> (string, string) flow

  val bound : int -> ('a, 'a) flow
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
  val map : ('a -> 'b) -> 'a stream -> 'b stream
  val of_bqueue : string Bqueue.t -> string stream
  val flat_map : ('a -> 'b stream) -> 'a stream -> 'b stream
  val to_file : string -> string stream -> unit
  val drain : 'a stream -> unit
  val each : ('a -> unit) -> 'a stream -> unit
end
