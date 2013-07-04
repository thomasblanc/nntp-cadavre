open Unix;;
open Nntp_helpers;;

type t = in_channel * out_channel * (string * string list) list;;
type id = string;;
type number = string;;
type article = string list;;

let mime_headers_text ~encoding =
["MIME-version: 1.0";("Content-type: text/plain; charset=\"" ^ encoding ^"\"")]

let get_caps inp out =
 write out "CAPABILITIES\r\n" ;
 flush out;
 let s = read_line inp in
 if ( String.sub s 0 3) = "101" then
 let r = get_multiline inp in
 List.map (fun x ->  let l = ( string_cut ' ' x) in (List.hd l, List.tl l) ) r
 else raise (Failure "Get caps");;

(* open the connexion to the specified server*)
let connection server port =
  let sock = socket PF_INET SOCK_STREAM 0 in
  let serv = gethostbyname server in
  let s = serv.h_addr_list.(0) in
  connect sock ( ADDR_INET (s, port));
  let (inp,out) = (*open_connection ( ADDR_INET ( s, port))*) (in_channel_of_descr sock, out_channel_of_descr sock) in
  let s = read_line inp in
  if ( String.sub s 0 3) = "200" then
    let cap_list = get_caps inp out in
    ( inp, out, cap_list)
  else raise ( Failure "Connecting")
 ;;

(* ask for reading permissions *)
let connection_as_reader server port =
 let (inp,out,cap_list) = connection server port in
 let b = has_mode cap_list "MODE-READER" in
 if b then
 (
  write out "MODE READER\r\n" ;
  flush out;
  let s = read_line inp in
  let code = ( String.sub s 0 3) in
  if code = "200" || code = "201" then
    let c = get_caps inp out in
     ( inp, out, c)
  else raise ( Failure "unable to switch to reader mode")
 )
 else
  (inp,out,cap_list);;

let authentificate (inp,out,cap_list) user pass =
 let b = has_mode_arg cap_list "AUTHINFO" "USER" in
 if b then
 (
  write out ("AUTHINFO USER " ^ user ^ "\r\n") ;
  flush out;
  let s = read_line inp in
  if check_code "381" s then
  (
   write out ("AUTHINFO PASS " ^ pass ^ "\r\n") ; 
   flush out;
   let s = read_line inp in
   let code = ( String.sub s 0 3) in
   if code <> "281"
    then raise ( Failure "pad_password")
    else (inp,out,get_caps inp out)
  )
  else raise ( Failure "bad_login")
 )
 else (inp,out,cap_list)
 ;;

let group (inp,out,_) group_name =
 write out ("GROUP "^group_name^endl) ;
 flush out;
 Server.parse_listgroup ( read_line inp);;


let list_group (inp, out, _) group_name range =
 write out ("LISTGROUP "^group_name^" "^range^endl) ;
 flush out;
 let s = read_line inp in
 let ( number, low, high, group) = Server.parse_listgroup s in
 ((number,low,high,group),get_multiline inp);;

let close (_,out,_) =
  write out "QUIT\r\n";flush out;
  (*shutdown_connection inp;
  close_in inp*)
  close (descr_of_out_channel out)
;;

let article_number (inp,out,_) number =
 write out ("ARTICLE " ^ number ^ "\r\n") ;
 flush out; 
 let s = read_line inp in
 let (_,i) = Server.parse_article s
 and art = get_multiline inp in
  (i,art);;

let article_id (inp,out,_) id =
 write out ("ARTICLE " ^ id ^ "\r\n") ;
 flush out; 
 let s = read_line inp in
 let _ = Server.parse_article s
 and art = get_multiline inp in
  art;;

let post ~from ~groups ~subject ?(date=Unix.gmtime ( Unix.time () )) ?(additionnal_headers=[]) ~body ~server_name (inp,out,_) =
 write out "POST\r\n" ;
 flush out;
 let s = read_line inp in
 let id = Server.parse_post server_name s in
 let w = write out in
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
 flush out;
 w ( String.concat endl additionnal_headers)  ;
 w endl ;
 w endl ;
 w (dot_stuff body) ;
 w "\r\n.\r\n" ;
 flush out;
 let s = read_line inp in
 let code = ( String.sub s 0 3) in
 if code <> "240" then raise ( Failure "post") else  ();;

let newnews ~wildmat ~tm (inp,out,caps) =
 if not ( has_mode caps "NEWNEWS") then raise ( Failure "newnews not implemented")
 else
 let s = string_of_int in
 write out ("NEWNEWS " ^ Wildmat.to_string wildmat ^" "^(s tm.tm_year)^(s tm.tm_mon)^(s tm.tm_mday)^" "^(s tm.tm_hour)^(s tm.tm_min)^(s tm.tm_sec)^" GMT\r\n");
 flush out;
 let s = read_line inp in
 if check_code "230" s then
  get_multiline inp
 else raise ( Failure "newnews unknown error");;

open Article

let post_reply ~id ~from ?groups ?subject ?date ?(additionnal_headers=[]) ~body ~server_name (inp,out,caps) =
 write out ( "HEAD " ^ id ^ endl);
 flush out;
 let s = read_line inp in
 if check_code "221" s then
 begin
  let rec aux res = function
    h::t -> aux (try (parse_header h)::res with _ -> res) t
  | [] -> res in
  let h = aux [] ( get_multiline inp) in
  let groups =
  match groups with
    Some g -> g
  | None ->
    try
     match List.find (fun x -> match x with Fu2 _ -> true | _ -> false) h with
     Fu2 g -> [g] | _ -> raise Not_found
    with Not_found ->
    try
     match List.find (fun x -> match x with Newsgroups _ -> true | _ -> false) h with
     Newsgroups g -> [g] | _ -> raise Not_found
    with Not_found -> raise ( Failure "post_reply: no group to post")

  and subject = match subject with
    Some s -> s
  | None ->
    try
     match List.find (fun x -> match x with Subject _ -> true | _ -> false) h with
     Subject s -> "Re: "^s | _ -> raise Not_found
    with Not_found -> raise ( Failure "post_reply: no sujbect")
  
  and refs =
  try
  match List.find (fun x -> match x with References _ -> true | _ -> false) h with
    References r -> "References: "^r^" "^id | _ -> raise Not_found
  with Not_found -> "References: "^id
  
  in
  match date with
    Some date -> post ~from ~groups ~subject ~date ~additionnal_headers:(refs::additionnal_headers) ~body ~server_name (inp,out,caps)
  | None -> post ~from ~groups ~subject ~additionnal_headers:(refs::additionnal_headers) ~body ~server_name (inp,out,caps)
    
  
 end
 else if check_code "430" s then raise ( Failure "post_reply: no such id") else raise ( Failure "post_reply: unknown failure");;
