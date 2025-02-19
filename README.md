# νιϝ, a small framework for building a web server from an OCaml script

(nu)(iota)(digamma)

**disclaimer**: Please note that this is an experimental project. It's also an
opportunity to build something that can be satisfying for web development.
However, we do not recommend using this project in production.

Vif is a small program that runs an OCaml script and launches a Web server from
it. The main idea is to be able to set up a typed Web server as quickly as
possible (note that we use [hurl][hurl], an HTTP client in OCaml)
```ocaml
$ opam pin add -y https://github.com/robur-coop/vif
$ opam pin add -y https://github.com/robur-coop/hurl
$ opam install vif hurl
$ cat >main.ml <<EOF
#require "vif" ;;

let default req target server () =
  let headers = [ "content-type", "text/html" ] in
  Vif.Response.with_string ~headers server `OK "Hello World!\n"
;;

let () =
  Miou_unix.run @@ fun () ->
  Vif.run ~default [] ()
;;
EOF
$ vif --pid vif.pid main.ml &
$ hurl http://localhost:8080/
HTTP/1.1 200 OK

connection: close
content-length: 13
content-type: text/html

Hello World!

$ kill -SIGINT $(cat vid.pid)
```

[hurl]: https://github.com/robur-coop/hurl
