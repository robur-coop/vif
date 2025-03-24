(** {1:bqueue Bounded queue.}

    A bounded queue is a queue that can be shared between two tasks (whether
    they are parallel or competing). One task can be a consumer and the other a
    producer. The bounded queue has a fixed size of elements that it can hold.
    If the producer fills the queue without the consumer consuming it, the
    producer will ultimately "block" until the consumer has at least consumed
    one value from the queue. *)

module Bqueue : sig
  type 'a t

  val create : int -> 'a t
  val put : 'a t -> 'a -> unit
  val get : 'a t -> 'a option
  val close : 'a t -> unit
end

(** {1:sources Sources.}

    Sources are decoupled producer of values.

    Elements are pulled from a source when needed. A source can have an internal
    state that will be lazily initialized when (and if) a consumer requests
    elements. The internal state will be safely disposed when the source runs
    out of elements, when the consumer terminates, or if an exception is raised
    at any point in the streaming pipeline.

    Sources are a great way to define decoupled producers that can be consumed
    with {!val:Stream.from}.

    The following example creates a source that counts down to zero:

    {[
      let count_down n =
        let init () = n in
        let pull i = if i = 0 then None else Some (i, i - 1) in
        let stop _ = () in
        Vif.S.Source { init; pull; stop }
    ]}

    It can be consumed with:

    {[
      # Vif.S.(Stream.from (count_down 3) |> into Sink.sum)
      - : int = 6
    ]}

    Sources are "single shot" amd will haver their input exhausted by most
    operations. *)

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

(** {1:sinks Sinks.}

    Sinks are decoupled consumer of values.

    Sinks are streaming abstractions that consume values and produce an
    aggregated value as a result. The result value is extracted from an internal
    state that is built incrementally. The internal state can acquire resources
    that are guaranteeed to be terminated when the sink is filled.

    Sinks are a great way to define decoupled consumers that can be filled with
    {!val:Stream.into}.

    The following example demonstrates a sink that consumes all elements into a
    list:

    {[
      let list =
        let init () = [] in
        let push acc x = x :: acc in
        let stop acc = List.rev acc in
        let full _ = false in
        Vif.S.Sink { init; push; full; stop }
    ]}

    Sinks are independent from sources and streams. You can think of them as
    packed arguments for folding functions with early termination. *)

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

(** {1:flows Flows.}

    Flows are decoupled transformers of values.

    Flows define streaming transformation, filtering or grouping operations that
    are fully disconnected from input and output. Their implementation
    intercepts an internal folding function and modifies the input one value at
    a time.

    Flows are great way to define decoupled transformations that can be used
    with {!val:Stream.via}.

    A flow can be applied to a stream with {!val:Stream.via}:

    {[
      # Stream.range 10 100
        |> Stream.via (Flow.map (fun x -> x + 1))
        |> Stream.into Sink.sum
      - : int = 4995
    ]}

    Flows can also be composed to form a pipeline:

    {[
      # let a = Flow.map (fun x -> x + 1) in
        let b = Flow.filter (fun x -> x mod 2 = 0) in
        Stream.range 10 100
        |> Stream.via Flow.(a >> b)
        |> Stream.into Sink.sum
      - : int = 2475
    ]} *)

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
