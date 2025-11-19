  $ export MIOU_DOMAINS=2
  $ export PORT=9457
  $ vif --pid-file vif.pid examples/01-hello/main.ml -p $PORT &
  $ ./waitfile.exe vif.pid
  $ hurl http://localhost:$PORT/ -p=b
  Hello World!
  $ kill -INT $(cat vif.pid)
  $ vif --pid-file vif.pid examples/02-counter/main.ml -p $PORT &
  $ ./waitfile.exe vif.pid
  $ hurl http://localhost:$PORT/ > /dev/null
  $ hurl http://localhost:$PORT/ > /dev/null
  $ hurl http://localhost:$PORT/ > /dev/null
  $ hurl http://localhost:$PORT/ > /dev/null
  $ hurl http://localhost:$PORT/ -p=b
  5 request(s)
  $ kill -INT $(cat vif.pid)
  $ vif --pid-file vif.pid examples/05-json/main.ml -p $PORT &
  $ ./waitfile.exe vif.pid
  $ hurl -m POST http://localhost:$PORT/ -p=b username=dinosaure password=foo age:=42
  username: dinosaure, password: foo, age: Some 42, address: None

  $ kill -INT $(cat vif.pid)
  $ vif --pid-file vif.pid examples/07-deflate/main.ml -p $PORT &
  $ ./waitfile.exe vif.pid
  $ hurl http://localhost:$PORT/deflate Accept-Encoding:deflate -p=h
  transfer-encoding: chunked
  content-type: text/plain; charset=utf-8
  content-encoding: deflate
  connection: close
  $ hurl http://localhost:$PORT/gzip Accept-Encoding:gzip -p=h
  transfer-encoding: chunked
  content-type: text/plain; charset=utf-8
  content-encoding: gzip
  connection: close
  $ kill -INT $(cat vif.pid)
  $ vif --pid-file vif.pid examples/10-route/main.ml -p $PORT &
  $ ./waitfile.exe vif.pid
  $ hurl http://localhost:$PORT/echo/dinosaure -p=b
  Hello, "dinosaure"!
  $ hurl http://localhost:$PORT/echo/foo -p=b
  Hello, "foo"!
  $ hurl http://localhost:$PORT/query foo==42 bar==dinosaure -p=b
  foo: 42 ("42")
  $ kill -INT $(cat vif.pid)
  $ cd examples/12-static/
  $ vif --pid-file vif.pid main.ml -p $PORT &
  $ ./../../waitfile.exe vif.pid
  $ hurl http://localhost:$PORT/index.html -p=h
  transfer-encoding: chunked
  etag: a127b2e10213e869311e4413f33ef0a8e5c69c2d407ca4056afedbabc695359d
  content-type: text/html
  content-length: 105
  $ kill -INT $(cat vif.pid)
  $ cd ../../

  $ vif --pid-file vif.pid examples/18-route-conv/main.ml -p $PORT &
  $ ./waitfile.exe vif.pid
This matches the "number" route:
  $ hurl http://localhost:$PORT/42 -p=b
  Hello World! The number is 42!
One might think this matches the "horse" route, but it is actually matched by
the "number" route - but the "number" route does not trigger as the converter
raises an exception through `int_of_string`!
  $ hurl http://localhost:$PORT/horse -p=b | head -n1
  Unspecified destination /horse (GET):
  $ kill -INT $(cat vif.pid)

  $ cd examples/19-regex-method/
  $ vif --pid-file vif.pid main.ml -p $PORT &
  $ ./../../waitfile.exe vif.pid
  $ hurl http://localhost:$PORT/ --method=GET -p=b greeting==Bob
  Hello GET Bob!
  $ hurl http://localhost:$PORT/ --multipart --method=POST -p=b greeting=Dylan
  Hello POST Dylan!
  $ kill -INT $(cat vif.pid)
  $ cd ../../
  $ vif examples/20-testing-uris/main.ml
  "/horse?q=mouth" matches: true
  "/horse/head?q=mouth&p=hoofs" matches: false
  Don't look a horse in the mouth.
  No match.
  `Converter failure: Invalid_argument("not a mouth")
