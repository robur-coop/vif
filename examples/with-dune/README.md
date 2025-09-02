# A simple chatroom with Vif and dune

This short example summarises our tutorial on how to use Vif and create a small
user space with a chat room. The tutorial is available [here][tutorial]. The
example uses `dune`, and the server can be launched with:

```shell
$ opam install vif jwto caqti-miou caqti-driver-sqlite3 tyxml-ppx brr js_of_ocaml
$ git clone https://github.com/robur-coop/vif
$ cd vif
$ dune build @example
...^C
```

The target creates an empty database and the necessary JavaScript script using
`js_of_ocaml`. The website can be accessed here: http://localhost:8080/. The
site is just one example of how to use Vif. Several aspects (such as security)
have not been fully developed.

[tutorial]: https://robur-coop.github.io/vif/
