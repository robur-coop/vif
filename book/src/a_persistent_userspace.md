# A persistent userspace

Vif is therefore a web framework, but it does not provide any means of
communicating with a database. However, when developing a website, it is
essential to be able to store information (such as user data) persistently and
independently of our HTTP server. Fortunately, another project allows
communication with a database: [caqti][caqti].

In this chapter, we will see how to communicate with a database using `caqti`
so that our users' information is stored outside the HTTP server and in a
persistent manner. To simplify this chapter, we will use [sqlite3][sqlite3]: a
simple file that represents our database (note that `caqti` can communicate
with other types of databases such as PostGreSQL).

## Create & use a database

Here we will create a simple database with a table called `users` containing
the user's username and password.

```shell
$ sqlite3 vif.db <<EOF
CREATE TABLE users (uid INTEGER, username TEXT, password TEXT, PRIMARY KEY(uid));
.quit
EOF
```

Next, we need to modify our Vif script so that we can read this database. To do
this, we will use `caqti-miou`, the `caqti` support with our Miou scheduler.

Next, we will need to explain to Vif how to create an instance that can
communicate with our database. Vif uses the concept of _devices_, which are
**global** instances available from our `server` instance and therefore
available in all our request handlers.

These _devices_ have the particularity of being _domain-safe_, meaning that two
domains can request these devices in parallel. We therefore need to ensure that
their manipulation is also _domain-safe_.

In this case, `caqti-miou` creates what is called a _connection pool_ to the
database. There is only one database, but several handlers can process SQL
requests in parallel (and require a connection to the database). Thanks to
`caqti`, we can obtain a `CONNECTION` to the database (in a _domain-safe_
manner) and, from this connection, make an SQL query (such as `SELECT`).

So let's first see how to create a Vif _device_ and how to use it:

```ocaml
#require "caqti-miou" ;;
#require "caqti-miou.unix" ;;
#require "caqti-driver-sqlite3" ;;

type env =
  { sw : Caqti_miou.Switch.t
  ; uri : Uri.t
  ; secret : string }
;;

let caqti =
  let finally pool = Caqti_miou_unix.Pool.drain pool in
  Vif.Device.v ~name:"caqti" ~finally [] @@ fun { sw; uri; _ } ->
  match Caqti_miou_unix.connect_pool ~sw uri with
  | Error err -> Fmt.failwith "%a" Caqti_error.pp err
  | Ok pool -> pool
;;

let users req server _ =
  let pool = Vif.Server.device caqti server in
  let sql =
    let open Caqti_request.Infix in
    Caqti_type.(unit ->! int) "SELECT COUNT(uid) FROM users" in
  let fn (module Conn : Caqti_miou.CONNECTION) = Conn.find sql () in
  match Caqti_miou_unix.Pool.use fn pool with
  | Ok n ->
    let open Vif.Response.Syntax in
    let* () = Vif.Response.add ~field:"content-type" "text/plain; charset= utf-8" in
    let* () = Vif.Response.with_string req (Fmt.str "%d user(s)!\n" n) in
    Vif.Response.respond `OK
  | Error err ->
    let open Vif.Response.Syntax in
    let str = Fmt.str "SQL error: %a" Caqti_error.pp err in
    let* () = Vif.Response.with_string req str in
    Vif.Response.respond `Internal_server_error
;;

let routes =
  let open Vif.Uri in
  let open Vif.Route in
  let open Vif.Type in
  [ post (m form) (rel / "login" /?? nil) --> login
  ; get (rel /?? nil) --> index
  ; get (rel / "users" /?? nil) --> users ]
;;

let () =
  Miou_unix.run @@ fun () ->
  Caqti_miou.Switch.run @@ fun sw ->
  let uri = Uri.make ~scheme:"sqlite3" ~path:"vif.db" () in
  let env = { sw; uri; secret= "deadbeef" } in
  let middlewares = Vif.Middlewares.[ jwt ] in
  let handlers = [ Vif.Handler.static ?top:None ] in
  let devices = Vif.Devices.[ caqti ] in
  Vif.run ~devices ~handlers ~middlewares routes env
;;
```

Here, we extend our `env` type to include the `caqti` switch and the `uri` to
our database. We then create our `caqti` device, which we will finally pass to
`Vif.run` (via the `devices` argument).

The `users` handler is an example of an SQL query that counts the number of
users. As you can see, we can retrieve our connection pool via
`Vif.Server.device caqti`. Finally, we need to use this connection pool to make
an SQL query, but we suggest you refer to the `caqti`
[documentation][caqti-documentation].

```shell
$ hurl http://localhost:8080/users
HTTP/1.1 200 OK

connection: close
content-length: 11
content-type: text/plain; charset= utf-8

0 user(s)!
```

Our database is empty, but this query confirms that we did indeed run an SQL
query to find out that it is empty! The client connected to our Vif server, the
server connected to our database, retrieved the information, processed it, and
then responded in text format that there are zero users.

## Verify passwords

We will add a new user _manually_ and improve our `login` function so that it
uses our database:

```shell
$ echo -n "42" | sha256sum
73475cb40a568e8da8a045ced110137e159f890ac4da883b6b17dc651b3a8049  -
$ sqlite3 vif.db <<EOF
INSERT INTO users (username, password) VALUES ('robur', '73475cb40a568e8da8a045ced110137e159f890ac4da883b6b17dc651b3a8049');
.quit
EOF
$ hurl http://localhost:8080/users -p b
1 user(s)!
```

Here, we can clearly see that the addition of a new user is taken into account
by our Vif server. It should be noted that we do not need to restart our Vif
server to obtain this response. This information is now stored in the database,
and our Vif server will simply (and each time) request the number of users. Now
let's re-implement the `login` function:

```ocaml
#require "digestif.c" ;;
#require "digestif" ;;

type user =
  { uid : int
  ; username : string 
  ; password : Digestif.SHA256.t }
;;

let user =
  let open Caqti_template.Row_type in
  let sha256 =
    let encode hash = Ok (Digestif.SHA256.to_hex hash) in
    let decode hex = Ok (Digestif.SHA256.of_hex hex) in
    custom ~encode ~decode string in
  product (fun uid username password -> { uid; username; password })
  @@ proj int (fun (t : user) -> t.uid)
  @@ proj string (fun (t : user) -> t.username)
  @@ proj sha256 (fun (t : user) -> t.password)
  @@ proj_end
;;

let login server (c : credentials) =
  let pool = Vif.Server.device caqti server in
  let sql =
    let open Caqti_request.Infix in
    Caqti_type.(string ->* user)
      "SELECT * FROM users WHERE username = ?" in
  let fn (module Conn : Caqti_miou.CONNECTION) = Conn.collect_list sql c.username in
  match Caqti_miou_unix.Pool.use fn pool with
  | Ok [ user ] ->
      let h = Digestif.SHA256.digest_string c.password in
      Digestif.SHA256.equal h user.password
  | _ -> false
;;

let login req server { secret } =
  let open Vif.Response.Syntax in
  match Vif.Request.of_multipart_form req with
  | Ok (({ username; _ } : credentials) as c) when login server c ->
    let token = Jwto.encode HS512 secret [ "username", username ] in
    let token = Result.get_ok token in
    let* () = Vif.Cookie.set ~name:"my-token" server req token in
    let* () = Vif.Response.add ~field:"content-type" "text/plain; charset=utf-8" in
    let* () = Vif.Response.with_string req "Authenticated!\n" in
    Vif.Response.redirect_to req Vif.Uri.(rel /?? any)
  | Ok _ ->
    let* () = Vif.Response.add ~field:"content-type" "text/plain; charset=utf-8" in
    let* () = Vif.Response.with_string req "Unauthorized!\n" in
    Vif.Response.respond `Unauthorized
  | Error _ ->
    let* () = Vif.Response.add ~field:"content-type" "text/plain; charset=utf-8" in
    let str = Fmt.str "Invalid multipart/form-data\n" in
    let* () = Vif.Response.with_string req str in
    Vif.Response.respond (`Code 422)
;;
```

We can see that `caqti` offers (using `Caqti_template`) more or less the same
DSL that we used with `jsont` or `multipart_form`. Using this DSL, we can
describe how to deserialise an SQL value to an OCaml value, which is what we do
with the `user` type. Next, we modify our first `login` function so that it
makes the SQL query to search for our user.

Passwords are _hashed_ using the SHA256 algorithm (thanks to the `digestif`
library). We will therefore hash the value given by the user and compare it
with what we have in the database.

Finally, the second `login` function changes very little; we just need to
change `when verify c` to `when login server c` so that our function can obtain
the `caqti` connection pool.

```shell
$ curl http://localhost:8080/login -F username=robur -F password=42 \
  --cookie-jar cookies.txt -L
<!DOCTYPE html>
<html xmlns="http://www.w3.org/1999/xhtml">
<head><title>My Vif Website!</title></head>
<body><p>Hello robur !</p></body>
</html>
```

We now have a user space that obtains user information from a database!

We would like to emphasise that throughout, we have attempted to type all
information (the password is no longer a string but a hash, which is a type
offered by [`digestif`][digestif]). The results of SQL queries are also typed
using Caqti. The main idea is always to prefer OCaml values (which have
undergone a whole series of checks upstream) to basic values that require a
whole ceremony to validate them (and, above all, to avoid bugs).

We will now focus on creating a new user. However, at this stage, I believe you
should be able to create such a page on your own.

## Create a new user

We will now implement a new page that will be a registration form. This page
will be associated with a POST route that will add the new user to our database
and redirect the client to our index page. We will reuse everything we have
just learned here. Let's start with the registration page `registration.html`:

```xml
<html>
<body>
<form action="/subscribe" method="post" enctype="multipart/form-data">
  <label for="username">Enter your username: </label>
  <input type="text" name="username" required />
  <label for="password">Enter your password: </label>
  <input type="password" name="password" required />
  <input type="submit" value="Subscribe!" />
</form>
</body>
</html>
```

Next, we will create a new POST route to register the user.

```ocaml
let subscribe req server _ =
  let pool = Vif.Server.device caqti server in
  let ( let* ) = Result.bind in
  let search =
    let open Caqti_request.Infix in
    Caqti_type.(string ->* user)
      "SELECT * FROM users WHERE username = ?" in
  let insert =
    let open Caqti_request.Infix in
    Caqti_type.(t2 string string ->. unit)
      "INSERT INTO users (username, password) VALUES (?, ?)" in
  let already_exists (c : credentials) (module Conn : Caqti_miou.CONNECTION) =
    let* user = Conn.collect_list search c.username in
    Ok (List.is_empty user == false) in
  let insert (c : credentials) (module Conn : Caqti_miou.CONNECTION) =
    let hash = Digestif.SHA256.digest_string c.password in
    let hash = Digestif.SHA256.to_hex hash in
    Conn.exec insert (c.username, hash) in
  let result =
    let* c = Vif.Request.of_multipart_form req in
    let* () =
      match Caqti_miou_unix.Pool.use (already_exists c) pool with
      | Ok true -> Error (`Already_exists c.username)
      | Ok false -> Ok ()
      | Error (#Caqti_error.t as err) -> Error err in
    match Caqti_miou_unix.Pool.use (insert c) pool with
    | Ok () -> Ok ()
    | Error (#Caqti_error.t as err) -> Error err in
  let open Vif.Response.Syntax in
  match result with
  | Ok () ->
      let* () = Vif.Response.empty in
      Vif.Response.redirect_to req Vif.Uri.(rel / "form.html" /?? any)
  | Error (`Already_exists username) ->
      let* () = Vif.Response.add ~field:"content-type" "text/plain; charset=utf-8" in
      let str = Fmt.str "%S already exists.\n" username in
      let* () = Vif.Response.with_string req str in
      Vif.Response.respond `Conflict
  | Error (#Caqti_error.t as err) ->
      let open Vif.Response.Syntax in
      let str = Fmt.str "SQL error: %a" Caqti_error.pp err in
      let* () = Vif.Response.with_string req str in
      Vif.Response.respond `Internal_server_error
  | Error (`Invalid_multipart_form | `Not_found _) ->
      let* () = Vif.Response.add ~field:"content-type" "text/plain; charset=utf-8" in
      let str = Fmt.str "Invalid multipart/form-data\n" in
      let* () = Vif.Response.with_string req str in
      Vif.Response.respond (`Code 422)
;;

let routes =
  let open Vif.Uri in
  let open Vif.Route in
  let open Vif.Type in
  [ post (m login_form) (rel / "login" /?? nil) --> login
  ; get (rel /?? nil) --> index
  ; get (rel / "users" /?? nil) --> users
  ; post (m login_form) (rel / "subscribe" /?? nil) --> subscribe ]
;;
```

In the code above, we reuse our `login_form` value, which describes our form.
This is because the form on the `register.html` page is the same as the one on
`login.html`.

Next, we check whether the user already exists. We therefore make an initial
SQL query and, if the user does not exist, we make an `INSERT` query. Finally,
we handle most error cases.

The code may be longer, but what is really interesting is how the `result`
value is calculated. If we look more closely, this function essentially
consists of SQL queries and returning errors in certain cases. This is where
Vif lets the developer choose how to organise the project.

One solution is to create a library containing the routes, SQL queries and
functions for displaying the results. But all this is outside the scope of Vif,
which is primarily intended to facilitate processes specific to managing HTTP
requests and producing responses.

We can now test our server:

```shell
$ hurl -m POST http://localhost:8080/subscribe --multipart username=foo password=bar
HTTP/1.1 303 See Other

location: /form.html
connection: close
content-length: 0
$ curl http://localhost:8080/login -F username=foo -F password=bar -L \
  --cookie-jar cookies.txt
<!DOCTYPE html>
<html xmlns="http://www.w3.org/1999/xhtml">
<head><title>My Vif Website!</title></head>
<body><p>Hello foo !</p></body>
</html>
```

Congratulations! You have created the basics of a user space using Vif. At this
point, we recommend that you explore the world of OCaml to see how to develop a
real web application. Vif focuses primarily on the server side, but there are
of course other aspects of web development that Vif does not handle (often
referred to as the front end).

## Final steps

We will conclude this tutorial by creating a _chat room_. Vif is capable of
managing websockets. This means that several authenticated clients can
communicate with each other in a shared space. This will allow us to explore
more complex but equally interesting features of Vif.

The current site can also be improved in many ways. For example, we can add an
email address (validated by [`emile`][emile]) and send a confirmation email
(using [`sendmail`][sendmail]) to complete the registration process.

We could also add an _avatar_ for our users and allow them to upload an image
that would be validated with `conan` and stored on our server.

In short, there are many possible ways to go at this stage, but the Vif
documentation is quite comprehensive and will provide you with all the
information you need.

[caqti]: https://github.com/paurkedal/ocaml-caqti
[sqlite3]: https://www.sqlite.org/
[caqti-documentation]: https://github.com/paurkedal/caqti-study/
[emile]: https://github.com/mirage/emile
[sendmail]: https://github.com/mirage/colombe
[digestif]: https://github.com/mirage/digestif
