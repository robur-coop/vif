  $ export MIOU_DOMAINS=2
  $ vif --pid-file vif.pid examples/01-hello/main.ml -p 8080 &
  $ ./waitfile.exe vif.pid
  $ hurl http://localhost:8080/
  HTTP/1.1 200 OK
  
  connection: close
  content-length: 13
  
  00000000: 4865 6c6c 6f20 576f 726c 6421 0a         Hello World!.
  
  $ kill -INT $(cat vif.pid)
  $ vif --pid-file vif.pid examples/02-counter/main.ml -p 8080 &
  $ ./waitfile.exe vif.pid
  $ hurl http://localhost:8080/ > /dev/null
  $ hurl http://localhost:8080/ > /dev/null
  $ hurl http://localhost:8080/ > /dev/null
  $ hurl http://localhost:8080/ > /dev/null
  $ hurl http://localhost:8080/
  HTTP/1.1 200 OK
  
  connection: close
  content-length: 13
  
  00000000: 3520 7265 7175 6573 7428 7329 0a         5 request(s).
  
  $ kill -INT $(cat vif.pid)
  $ vif --pid-file vif.pid examples/05-json/main.ml -p 8080 &
  $ ./waitfile.exe vif.pid
  $ hurl -m POST http://localhost:8080/ username=dinosaure password=foo age:=42
  HTTP/1.1 200 OK
  
  connection: close
  content-length: 64
  
  00000000: 7573 6572 6e61 6d65 3a20 6469 6e6f 7361  username: dinosa
  00000010: 7572 652c 2070 6173 7377 6f72 643a 2066  ure, password: f
  00000020: 6f6f 2c20 6167 653a 2053 6f6d 6520 3432  oo, age: Some 42
  00000030: 2c20 6164 6472 6573 733a 204e 6f6e 650a  , address: None.
  
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
