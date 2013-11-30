open Unix
open Nntp_helpers

type t = 
  {
    mutable get : in_channel;
    mutable send : out_channel;
    mutable caps : (string * string list) list;
  }

type id = string
type number = string
type article = string list

let mime_headers_text ~encoding =
  ["MIME-version: 1.0";("Content-type: text/plain; charset=\"" ^ encoding ^"\"")]

let get_caps c =
  write c.send "CAPABILITIES\r\n" ;
  flush c.send;
  let s = read_line c.get in
  if ( String.sub s 0 3) = "101"
  then
    let r = get_multiline c.get in
    c.caps <-
      List.map
        (fun x ->
           let l = ( string_cut ' ' x) in
           (List.hd l, List.tl l)
        )
        r
  else failwith "Get caps"


(* open the connexion to the specified server*)
let connection server port =
  let sock = socket PF_INET SOCK_STREAM 0 in
  let serv = gethostbyname server in
  let s = serv.h_addr_list.(0) in
  connect sock ( ADDR_INET (s, port));
  let c =
    {
      get = in_channel_of_descr sock;
      send = out_channel_of_descr sock;
      caps = [];
    }
  in
  let s = read_line c.get in
  if ( String.sub s 0 3) = "200"
  then ( get_caps c; c )
  else failwith "Connecting"


(* ask for reading permissions *)
let connection_as_reader server port =
  let c = connection server port in
  let b = has_mode c.caps "MODE-READER" in
  if b then
    (
      write c.send "MODE READER\r\n" ;
      flush c.send;
      let s = read_line c.get in
      let code = ( String.sub s 0 3) in
      if code = "200" || code = "201"
      then ( get_caps c; c )
      else failwith "unable to switch to reader mode"
    )
  else c

let authentificate c user pass =
  if has_mode_arg c.caps "AUTHINFO" "USER"
  then
    begin
      write c.send ("AUTHINFO USER " ^ user ^ "\r\n") ;
      flush c.send;
      let s = read_line c.get in
      if check_code "381" s then
        (
          write c.send ("AUTHINFO PASS " ^ pass ^ "\r\n") ; 
          flush c.send;
          let s = read_line c.get in
          let code = ( String.sub s 0 3) in
          if code <> "281"
          then failwith "pad_password"
          else get_caps c
        )
      else failwith "bad_login"
    end
  else ()


let group c group_name =
  write c.send ("GROUP "^group_name^endl) ;
  flush c.send;
  Server.parse_listgroup ( read_line c.get)


let list_group c group_name range =
  write c.send "LISTGROUP ";
  write c.send group_name;
  write c.send " ";
  write c.send range;
  write c.send endl;
  flush c.send;
  let s = read_line c.get in
  let ( number, low, high, group) = Server.parse_listgroup s in
  ((number,low,high,group),get_multiline c.get)

let close c =
  write c.send "QUIT\r\n";
  flush c.send;
  close (descr_of_out_channel c.send)

let article_number c number =
  write c.send "ARTICLE ";
  write c.send number;
  write c.send endl;
  flush c.send; 
  let s = read_line c.get in
  let (_,i) = Server.parse_article s
  and art = get_multiline c.get in
  (i,art)

let article_id c id =
  snd ( article_number c id )

let post ~from ~groups ~subject ?(date=Unix.gmtime ( Unix.time () )) ?(additionnal_headers=[]) ~body ~server_name c =
  write c.send "POST\r\n" ;
  flush c.send;
  let s = read_line c.get in
  let id = Server.parse_post server_name s in
  let w = write c.send in
  w "From: " ;
  w from ;
  w "\r\nNewsgroups: " ;
  w ( String.concat "," groups) ;
  w "\r\nSubject: " ;
  w subject ;
  w "\r\nMessage-ID: " ;
  w id ;
  w "\r\nDate: " ;
  w (from_gmtime date) ;
  w endl ;
  flush c.send;
  w ( String.concat endl additionnal_headers)  ;
  w endl ;
  w endl ;
  w (dot_stuff body) ;
  w "\r\n.\r\n" ;
  flush c.send;
  let s = read_line c.get in
  let code = ( String.sub s 0 3) in
  if code <> "240"
  then failwith "post"
  else  ()

let newnews ~wildmat ~tm c =
  if not ( has_mode c.caps "NEWNEWS")
  then failwith "newnews not implemented"
  else
    let s = string_of_int in
    write c.send "NEWNEWS ";
    write c.send ( Wildmat.to_string wildmat );
    write c.send " ";
    write c.send (s tm.tm_year);
    write c.send (s tm.tm_mon);
    write c.send (s tm.tm_mday);
    write c.send " ";
    write c.send (s tm.tm_hour);
    write c.send (s tm.tm_min);
    write c.send (s tm.tm_sec);
    write c.send " GMT\r\n";
    flush c.send;
    let s = read_line c.get in
    if check_code "230" s
    then get_multiline c.get
    else failwith "newnews unknown error"

open Article

let post_reply
    ~id ~from ?groups ?subject ?date ?(additionnal_headers=[])
    ~body ~server_name c =
  write c.send "HEAD ";
  write c.send id;
  write c.send endl;
  flush c.send;
  let s = read_line c.get in
  if check_code "221" s then
    begin
      let rec aux res = function
        | h::t ->
          aux
            (
              try
                (parse_header h)::res
              with
                _ -> res
            )
            t
        | [] -> res in
      let h = aux [] ( get_multiline c.get) in
      let groups =
        match groups with
          Some g -> g
        | None ->
          let rec find_group g = function
            | (Fu2 g)::_ -> [g]
            | (Newsgroups g)::tl -> find_group (Some g) tl
            | _ :: tl -> find_group g tl
            | [] ->
              (
                match g with
                | None -> failwith "post_reply: no group to post"
                | Some g -> [g]
              )
          in find_group None h


      and subject =
        match subject with
        | Some s -> s
        | None ->
          let rec aux = function
            | ( Subject s ) :: _ -> "Re: " ^ s
            | _ :: tl -> aux tl
            | [] -> failwith "post_reply: no sujbect"
          in
          aux h

      and refs =
        let rec aux = function
          | ( References r ) :: _ -> "References: "^ r ^" "^ id
          | _ :: tl -> aux tl
          | [] -> "References: " ^ id
        in aux h

      in
      match date with
      | Some date ->
        post
          ~from ~groups ~subject ~date
          ~additionnal_headers:(refs::additionnal_headers)
          ~body ~server_name
          c
      | None ->
        post
          ~from ~groups ~subject
          ~additionnal_headers:(refs::additionnal_headers)
          ~body ~server_name
          c
    end
  else
  if check_code "430" s
  then failwith "post_reply: no such id"
  else failwith "post_reply: unknown failure"
