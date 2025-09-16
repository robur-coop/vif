(* The MIT License

   Copyright (c) 2016 Jane Street Group, LLC <opensource@janestreet.com>
   Copyright (c) 2024 Romain Calascibetta <romain.calascibetta@gmail.com>
   
   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to deal
   in the Software without restriction, including without limitation the rights
   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
   copies of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:
   
   The above copyright notice and this permission notice shall be included in all
   copies or substantial portions of the Software.
   
   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE. *)

{
exception Lexical_error of string * string * int * int

type token =
  | Name   of string
  | String of string
  | Minus
  | Lparen
  | Rparen
  | Comma
  | Equal
  | Plus_equal
  | Eof

let escaped_buf = Buffer.create 256
let raise_lexical_error lexbuf fmt =
  let p = Lexing.lexeme_start_p lexbuf in
  Fmt.kstr (fun msg ->
    raise (Lexical_error (msg, p.Lexing.pos_fname,
                               p.Lexing.pos_lnum,
                               p.Lexing.pos_cnum - p.Lexing.pos_bol + 1)))
    fmt
}

rule token = parse
  | [' ' '\t' '\r']* { token lexbuf }
  | '#' [^ '\n']* { token lexbuf }
  | '\n' { Lexing.new_line lexbuf; token lexbuf }

  | ['A'-'Z' 'a'-'z' '0'-'9' '_' '.']+ as s { Name s }
  | '"'
      { Buffer.clear escaped_buf;
        string escaped_buf lexbuf }
  | '-' { Minus }
  | '(' { Lparen }
  | ')' { Rparen }
  | ',' { Comma }
  | '=' { Equal }
  | "+=" { Plus_equal }
  | eof { Eof }
  | _
    { raise_lexical_error lexbuf "illegal character %S"
        (String.escaped (Lexing.lexeme lexbuf)) }

and string buf = parse
  | '"'
      { String (Buffer.contents buf) }
  | "\\\n"
  | '\n'
      { Lexing.new_line lexbuf;
        Buffer.add_char buf '\n';
        string buf lexbuf }
  | '\\' (_ as c)
  | (_ as c)
      { Buffer.add_char buf c;
        string buf lexbuf }
  | eof { raise_lexical_error lexbuf "unterminated string" }
