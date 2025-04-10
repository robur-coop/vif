  $ export MIOU_DOMAINS=2
  $ vif --pid-file vif.pid examples/01-hello/main.ml -p 8080 &
  $ ./waitfile.exe vif.pid
  $ hurl http://localhost:8080/ -p=b
  Hello World!
  
  $ kill -INT $(cat vif.pid)
  $ vif --pid-file vif.pid examples/02-counter/main.ml -p 8080 &
  $ ./waitfile.exe vif.pid
  $ hurl http://localhost:8080/ > /dev/null
  $ hurl http://localhost:8080/ > /dev/null
  $ hurl http://localhost:8080/ > /dev/null
  $ hurl http://localhost:8080/ > /dev/null
  $ hurl http://localhost:8080/ -p=b
  5 request(s)
  
  $ kill -INT $(cat vif.pid)
  $ vif --pid-file vif.pid examples/05-json/main.ml -p 8080 &
  $ ./waitfile.exe vif.pid
  $ hurl -m POST http://localhost:8080/ -p=b username=dinosaure password=foo age:=42
  username: dinosaure, password: foo, age: Some 42, address: None
  

  $ kill -INT $(cat vif.pid)
  $ vif --pid-file vif.pid examples/07-deflate/main.ml -p 8080 &
  $ ./waitfile.exe vif.pid
  $ hurl http://localhost:8080/deflate Accept-Encoding:deflate -p=h
  transfer-encoding: chunked
  content-type: text/plain; charset=utf-8
  content-encoding: deflate
  connection: close
  $ hurl http://localhost:8080/gzip Accept-Encoding:gzip -p=h
  transfer-encoding: chunked
  content-type: text/plain; charset=utf-8
  content-encoding: gzip
  connection: close
  $ kill -INT $(cat vif.pid)
  $ vif --pid-file vif.pid examples/10-route/main.ml -p 8080 &
  $ ./waitfile.exe vif.pid
  $ hurl http://localhost:8080/echo/dinosaure -p=b
  Hello, "dinosaure"!
  
  $ hurl http://localhost:8080/echo/foo -p=b
  Hello, "foo"!
  
 
  $ hurl http://localhost:8080/query foo==42 bar==dinosaure -p=b
  foo: 42 ("42")
  
  $ kill -INT $(cat vif.pid)
  $ cd examples/12-static/
  $ vif --pid-file vif.pid main.ml -p 8080 &
  $ ./../../waitfile.exe vif.pid
  $ hurl http://localhost:8080/index.html -p=h
  transfer-encoding: chunked
  etag: a127b2e10213e869311e4413f33ef0a8e5c69c2d407ca4056afedbabc695359d
  content-length: 105
  content-type: text/html
  $ kill -INT $(cat vif.pid)
  $ cd ../../
