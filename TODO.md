- [x] be able to load *.cmx{,a} when we use "#require"
  `findlib`/`topfind` only loads directories, `ocamlnat` wants to load files.
  Let's use our work about `uniq` to solve dependencies and load artifacts
  + [ ] lint the way we load dependencies
  + [ ] compare to what `omod` can do (may be a better approach)
- [x] do some tests with Miou and see if we can execute small applications
- [x] start to eval a file (and show the result?)
  + [ ] do we need to show the result?
- [x] start to make a nice intf for a HTTP server via `httpcats`
  + [x] provide Request
  + [x] provide Response
  + [x] provide Method
  + [x] provide Status
- [ ] dispatch on method and content-type
  + [x] recognize application/json
  + [x] recognize multipart/form
    * [x] provide an API to describe, by types, a multipart/form
      - [ ] add some basic types like `int`
- [x] be able to extract the body of a request as a JSON value
  + [x] cast a JSON value to an OCaml value via Json_encoding (or repr?)
  + [ ] lint this function and errors (???)
- [ ] template engine with jingoo or eml?
  + [ ] with jingoo, do something "à la printf" and provide a tool which generates
        from a string something like `(_, _, _, _) format4`
  + [ ] jingoo to GADT?
- [x] be able to use ppx into our script
  + [x] use tyxml and emit it as a stream
- [ ] websocket
- [ ] be able to specify certificates in options
