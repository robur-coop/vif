# A chatroom with websockets and JavaScript

There is one last way to communicate with our HTTP server: the
[WebSocket][websocket] protocol. The advantage of this protocol is that the
connection is full-duplex: throughout the entire process, the user can
communicate with you and you can continuously communicate with them. It's like
a persistent communication.

In short, a good example of the use of WebSockets is a chat room. We want users
to be able to communicate with other users in real time. We will therefore
initiate WebSocket connections between these users in order to _multiplex_ the
messages.

First, we will create a fairly basic page where users can send messages to each
other:

```xml
<html>
<head>
  <meta charset="utf-8">
  <script type="text/javascript" defer="defer" src="chat.js"></script>
  <title>Chat room</title>
</head>
<body>
  <form id="send">
    <input type="text" name="msg" required />
    <input type="submit" value="Send!" />
  </form>
</body>
</html>
```

Here we add a file called `chat.js`, which will be the result of compiling an
OCaml file using `js_of_ocaml`. This small JavaScript script has two objectives:
1) connect to the server via WebSocket and receive messages to write on the page
2) send messages as soon as the user clicks on the `Send!` button.

To implement all this, we will use [`brr`][brr], a library that interfaces
JavaScript functions in OCaml:

```ocaml
let form_elem = Brr.El.find_first_by_selector (Jstr.v "#send") |> Option.get

let send_message ev_submit =
  let open Fut.Syntax in
  let _ = Jv.call (Brr.Ev.to_jv ev_submit) "preventDefault" [||] in
  let form_elem = Brr_io.Form.of_el form_elem in
  let form_data = Brr_io.Form.Data.of_form form_elem in
  let body = Brr_io.Fetch.Body.of_form_data form_data
  and credentials = Brr_io.Fetch.Request.Credentials.same_origin in
  let init = Brr_io.Fetch.Request.init
    ~body ~credentials ~method':(Jstr.v "POST") () in
  let req = Brr_io.Fetch.Request.v ~init (Jstr.v "http://localhost:8080/send") in
  Fut.await begin
    let* result = Brr_io.Fetch.request req in
    match result with
    | Ok resp when Brr_io.Fetch.Response.ok resp -> Fut.return ()
    | Ok resp -> print_endline "Error!"; Fut.return ()
    | Error _ -> print_endline "Error!"; Fut.return ()
  end @@ Fun.id

let on_message ev =
  let msg = Jv.Jstr.get (Brr.Ev.to_jv ev) "data" in
  let div = Brr.El.(div [txt msg]) in
  Brr.El.append_children (Brr.Document.body Brr.G.document) [div]

let () =
  let socket = Brr_io.Websocket.create (Jstr.v "http://localhost:8080/chat") in
  let target = Brr_io.Websocket.as_target socket in
  let event = Brr.Ev.Type.create (Jstr.v "message") in
  ignore (Brr.Ev.listen event on_message target);
  let target = Brr.El.as_target form_elem in
  ignore (Brr.Ev.listen Brr_io.Form.Ev.submit send_message target)
```

The code may seem complex, and we could write the equivalent in JavaScript, but
let's stay in the world of OCaml. The goal here is to retrieve our form and be
able to read what the user has written as soon as they press the "Send!"
button.

Next, we initiate a WebSocket connection with our server (taking care to keep
our cookies so that we can remain authenticated).

Finally, for each message received from the WebSocket, we will simply write it
_on the fly_ on the page.

The JavaScript code can be obtained in this way:
```shell
$ opam install brr js_of_ocaml
$ ocamlfind c -linkpkg -package brr chat.ml
$ js_of_ocaml a.out -o chat.js
```

That's all we need on the client side. We will now return to Vif to:
1) propose a _handler_ to the WebSocket protocol
2) and create a new POST route to send messages

## Our listeners on our Vif server

The idea behind our chatroom is quite simple. When someone connects to our
server via websocket, we create a _listener_ in the sense that the client will
listen for any new messages.

This listener will be stored in a global value so that it can be retrieved from
any request handlers. As such, access must be protected so that this global
value is _domain-safe_.

There are three essential operations:
1) create a listener
2) send a message to all listeners (i.e. all connected clients)
3) delete a listener

```ocaml
type chatroom =
  { make : unit -> int * string Vif.Stream.source
  ; send : string -> unit
  ; shutdown : int -> unit }
;;

let chatroom =
  let uid = Atomic.make 0 in
  let actives = Hashtbl.create 0x100 in
  let mutex = Miou.Mutex.create () in
  let make () =
    let n = Atomic.fetch_and_add uid 1 in
    Miou.Mutex.protect mutex @@ fun () ->
    let q = Vif.Stream.Bqueue.create 0x100 in
    Hashtbl.replace actives n q;
    n, Vif.Stream.Source.of_bqueue q in
  let shutdown uid =
    Miou.Mutex.protect mutex @@ fun () ->
    Hashtbl.remove actives uid in
  let send msg =
    Miou.Mutex.protect mutex @@ fun () ->
    let fn _ q = Vif.Stream.Bqueue.put q msg in
    Hashtbl.iter fn actives in
  let finally _ =
    Miou.Mutex.protect mutex @@ fun () ->
    let fn _ q = Vif.Stream.Bqueue.close q in
    Hashtbl.iter fn actives in
  Vif.Device.v ~name:"chatroom" ~finally [] @@ fun _ ->
  { make; send; shutdown }
;;
```

Here, we introduce two new concepts: a bounded queue and streams. We won't go
into detail about these modules, but they allow information to be transmitted
(and it's always _domain-safe_) between tasks. Conceptually, several tasks
(probably dispatched across several domains) will run to _listen_ for any
messages we might want to send. A task will then appear that will execute the
handler for the POST request (allowing messages to be sent) and will have to
transmit this message to all active listeners (this is the purpose of the
`send` function).

Our chatroom, being global to our server, will be a device. We will then create
the WebSocket handler and a final route to be able to send messages.

## Websocket

The WebSocket protocol is a protocol that can be initiated from an HTTP
request. It involves creating a route and informing the client that we would
like to switch to the WebSocket protocol rather than HTTP, which is called an
_upgrade_. Vif allows you to attempt this upgrade. The client will then be
redirected to another handler, the WebSocket handler. This handler is special
because it no longer processes a request and provides a response, but works
with a stream of inputs (`ic`) and a stream of outputs (`oc`).

```ocaml
let chat req server _ = Vif.Response.websocket ;;

let websocket ic oc server _ =
  let t = Vif.Server.device chatroom server in
  let uid, src = t.make () in
  let fn str = oc (`Msg (`Text, true), str) in
  let prm0 = Miou.async @@ fun () -> Vif.Stream.Source.each fn src in
  let prm1 = Miou.async @@ fun () ->
    let rec go () =
      match ic () with
      | None | Some (`Connection_close, _) -> oc (`Connection_close, String.empty)
      | Some _ -> go () in
    go () in
  let _ = Miou.await_first [ prm0; prm1 ] in
  t.shutdown uid
;;
```

Here, we introduce a few concepts related to Miou. When a client connects to
our websocket, the goal is to create tasks that will work together:
1) one will consume everything the client can send (and it should not send
   anything normally)
2) the other task will consist of transmitting messages from our _listener_ to
   our client

`Vif.Stream.Source.each` will execute `fn` as soon as the listener receives a
message, and `fn` will simply write this message to the client using `oc`.

One or both of these tasks will stop (because the client has disconnected or
because we want to shutdown the server). What is certain is that in any case,
everything must end. `Miou.await_first` will wait for one of the tasks and,
more importantly, will _cancel_ the other. We can finally release the listener
resource correctly.

It may be interesting to look at Miou at this point and how it manages tasks.
We can recommend [a short book][miou-book] that explains in detail what a
scheduler looks like in OCaml 5 (with effects) and what Miou offers.

## Send a message!

Here, we will create a new handler, which will be our last one and will
summarise everything we have learned since the beginning of this short book. It
is a handler for a POST request in which we would have our message (`msg`). It
is still a question of whether the client is connected, and we will simply
prefix the message with the client's name.

```ocaml
type msg = { msg : string } ;;

let msg =
  let open Vif.Multipart_form in
  let fn msg = { msg } in
  record fn
  |+ field "msg" string
  |> sealr
;;

let send req server _ =
  let open Vif.Response.Syntax in
  match Vif.Request.of_multipart_form req, Vif.Request.get jwt req with
  | Ok { msg }, Some { username }->
    let t = Vif.Server.device chatroom server in
    let str = Fmt.str "%s: %s\n" username msg in
    t.send str;
    let* () = Vif.Response.add ~field:"content-type" "text/plain; charset=utf-8" in
    let* () = Vif.Response.with_string req "Message sent!\n" in
    Vif.Response.respond `OK
  | Error _, _ ->
    let* () = Vif.Response.add ~field:"content-type" "text/plain; charset=utf-8" in
    let str = Fmt.str "Invalid multipart/form-data\n" in
    let* () = Vif.Response.with_string req str in
    Vif.Response.respond (`Code 422)
  | _, None ->
    let* () = Vif.Response.add ~field:"content-type" "text/plain; charset=utf-8" in
    let* () = Vif.Response.with_string req "Unauthorized!" in
    Vif.Response.respond `Unauthorized
;;
```

Note that we are reusing our `chatroom` device here. This does not involve
creating a listener as in our WebSocket handler, but rather _broadcasting_ the
message to all listeners.

## Mix them all!

We now need to properly configure our new `chatroom` device in Vif and give it
the handler for WebSocket connections:

```ocaml
let routes =
  let open Vif.Uri in
  let open Vif.Route in
  let open Vif.Type in
  [ post (m login_form) (rel / "login" /?? nil) --> login
  ; get (rel /?? nil) --> index
  ; get (rel / "users" /?? nil) --> users
  ; post (m login_form) (rel / "subscribe" /?? nil) --> subscribe
  ; get (rel / "chat" /?? nil) --> chat
  ; post (m msg) (rel / "send" /?? nil) --> send ]
;;

let () =
  Miou_unix.run @@ fun () ->
  Caqti_miou.Switch.run @@ fun sw ->
  let uri = Uri.make ~scheme:"sqlite3" ~path:"vif.db" () in
  let env = { sw; uri; secret= "deadbeef" } in
  let middlewares = Vif.Middlewares.[ jwt ] in
  let handlers = [ Vif.Handler.static ?top:None ] in
  let devices = Vif.Devices.[ caqti; chatroom ] in
  Vif.run ~devices ~handlers ~middlewares ~websocket routes env
;;
```

There you go! You can now access the page [http://localhost:8080/chat.html][l]
and, if you are logged in, you can send a message that others will be able to
read. It's a real chat room made with Vif.

## Conclusion

Of course, there is room for improvement (starting with the design!). But the
bulk of the logic, the backend, is there. Despite Vif's minimalism, it is
possible to achieve satisfactory results fairly quickly.

Vif offers a way to develop websites with OCaml by taking up the idea of OCaml
scripts. Fortunately, this is not the central idea (note the use of effects
with Miou, the possibility of parallelising request management with OCaml 5,
etc.). The idea of scripting in OCaml is interesting because it requires very
little to get a website up and running.

Finally, Vif attempts to offer, at all levels of the HTTP protocol, a way of
typing information so that all checks can be performed upstream using DSLs such
as `jsont` or `caqti`. The idea is to really take advantage of the OCaml type
system (and see it more as an assistant rather than a constraint).

[brr]: https://github.com/dbuenzli/brr
[websocket]: https://en.wikipedia.org/wiki/WebSocket
[miou-book]: https://robur-coop.github.io/miou/
[l]: http://localhost:8080/chat.html
