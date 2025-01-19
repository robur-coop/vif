type 'a t = {
    buf: 'a option array
  ; mutable rd_pos: int
  ; mutable wr_pos: int
  ; mutable closed: bool
  ; mutex: Miou.Mutex.t
  ; non_empty_or_close: Miou.Condition.t
  ; non_full: Miou.Condition.t
}

let create len =
  {
    buf= Array.make len None
  ; rd_pos= 0
  ; wr_pos= 0
  ; closed= false
  ; mutex= Miou.Mutex.create ()
  ; non_empty_or_close= Miou.Condition.create ()
  ; non_full= Miou.Condition.create ()
  }

let put t value =
  Miou.Mutex.protect t.mutex @@ fun () ->
  if t.closed then invalid_arg "Stream.put: closed stream";
  while (t.wr_pos + 1) mod Array.length t.buf = t.rd_pos do
    Miou.Condition.wait t.non_full t.mutex
  done;
  t.buf.(t.wr_pos) <- Some value;
  t.wr_pos <- (t.wr_pos + 1) mod Array.length t.buf;
  Miou.Condition.signal t.non_empty_or_close

let get t =
  Miou.Mutex.protect t.mutex @@ fun () ->
  while t.wr_pos = t.rd_pos && not t.closed do
    Miou.Condition.wait t.non_empty_or_close t.mutex
  done;
  if t.wr_pos = t.rd_pos && t.closed then None
  else begin
    let value = t.buf.(t.rd_pos) in
    t.buf.(t.rd_pos) <- None;
    t.rd_pos <- (t.rd_pos + 1) mod Array.length t.buf;
    Miou.Condition.signal t.non_full;
    value
  end

let close t =
  Miou.Mutex.protect t.mutex @@ fun () ->
  t.closed <- true;
  Miou.Condition.signal t.non_empty_or_close
