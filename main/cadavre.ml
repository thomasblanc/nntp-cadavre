(*********************************)
(*                               *)
(*         Cadavre.ml            *)
(*                               *)
(*********************************)

(** Inspiré du script d'A. Loyer, dit Drébon,
le cadavre permet d'animer des news avec des petites phrases personnalisables.*)

type data = Data.t;;
open Data

let _ = Random.self_init ();;

type id = Nntp.id;;

let c () =
  let c = Nntp.connection_as_reader Config.server Config.port in
  if Config.login <> "" && Config.password <> ""
  then Nntp.authentificate c Config.login Config.password
  else c;;

let post ~subject ~body =
 let c = c () in
 Nntp.post ~from:Config.from ~groups:[Config.group] ~subject ~additionnal_headers:(Nntp.mime_headers_text ~encoding:"utf8") ~body ~server_name:Config.server c;
 Nntp.close c;;
let rep ~id ~body =
 let c = c () in
 Nntp.post_reply ~id ~from:Config.from ~additionnal_headers:(Nntp.mime_headers_text ~encoding:"utf8") ~body ~server_name:Config.server c;
 Nntp.close c;;

let ask_last_number = Data.ask_last_number;;

let data =
  try 
    Data2.get_data Config.datad
  with _ ->
    try
      let f = open_in_bin Config.data in
      let d = Marshal.from_channel f in
      close_in f; d
    with _ ->
      let t = Unix.time () in
      {
        last_post = 0;
        last_seen_number = ask_last_number ();
        last_time = (t, Unix.localtime t);
        subjects = [|("Le cadavre",2)|];
        verbs =
          [|[|
            [|"connais";"connais";"connait";"connaissons";"connaissez";"connaissent"|];
            [|"connaissais";"connaissais";"connaissait";"connaissions";"connaissiez";"connaissaient"|];
            [|"connaîtrai";"connaîtras"; "connaîtra";"connaîtrons";"connaîtrez";"connaitront"|];
            [|"ai connu";"as connu";"a connu";"avons connu";"avez connu";"ont connu"|]
          |]|];
        comps = [|"une seule phrase"|];
        dates = [|("dans la phase de test",0)|];
        places = [|"sur les news"|];
      }


let add_time (t,tm) =
 (*Printf.printf "New possible time %f\r\n" t; flush Pervasives.stdout;*)
 if t > fst data.last_time then ( (*print_endline "ok !";*)data.last_time <- (t,tm));;
let add_subject ~id s i =
 data.subjects <- Array.append data.subjects [|(s,pred i)|];
  rep ~id ~body:("J'ai apris mon "^(string_of_int (Array.length data.subjects))^"e sujet\r\n-- \r\n"^Config.signature);;
let add_verb ~id v =
 data.verbs <- Array.append data.verbs [|v|];
 rep ~id ~body:("J'ai apris mon "^(string_of_int (Array.length data.verbs))^"e verbe\r\n-- \r\n"^Config.signature);;
let add_comp ~id c =
 data.comps <- Array.append data.comps [|c|];
 rep ~id ~body:("J'ai apris mon "^(string_of_int (Array.length data.comps))^"e complément\r\n-- \r\n"^Config.signature);;
let add_date ~id d i =
 data.dates <- Array.append data.dates [|(d,pred i)|];
 rep ~id ~body:("J'ai apris ma "^(string_of_int (Array.length data.dates))^"e date\r\n-- \r\n"^Config.signature);;
let add_place ~id p =
 data.places <- Array.append data.places [|p|];
 rep ~id ~body:("J'ai apris mon "^(string_of_int (Array.length data.places))^"e lieu\r\n-- \r\n"^Config.signature);;

let save_last_number n = data.last_seen_number <- n
let last_seen_number () = data.last_seen_number


let save_data () =
  if Config.data <> ""
  then
    begin
      let f = open_out_bin Config.data in
      Marshal.to_channel f data [];
      close_out f
    end;
  if Config.datad <> ""
  then Data2.save_data data Config.datad

let send_help id = rep ~id ~body:(Config.help_msg^"\r\n-- \r\n"^Config.signature);;

let last_time () = data.last_time;;

(* cadavre generating : *)

let endl = "\r\n";;

let random_choose a = a.( Random.int ( Array.length a));;
let sentence () =
 let (s,i) = random_choose data.subjects
 and (d,t) = random_choose data.dates in
 let v = (random_choose data.verbs).(t).(i)
 and c = random_choose data.comps
 and p = random_choose data.places
 in
 (s,v,c,d,p);;

let post_sentence () =
 let subject = Printf.sprintf "[%d] %s" data.last_post Config.subject
 and (s,v,c,d,p) = sentence () in
 let body = Printf.sprintf "%s\r\n%s\r\n%s\r\n%s\r\n%s\r\n%s.\r\n%s\r\n-- \r\n%s"
  Config.msg_begin s v c d p Config.msg_end Config.signature in
 data.last_post <- succ data.last_post;
 post ~subject ~body;;

let is_bored () =
  let curr = Unix.time () and last = (fst data.last_time) in
  let diff = (curr-.last) in
  Printf.printf "Current time %f, last time %f, configurated for %f, diff is %d\r\n" curr last Config.bored (int_of_float diff); flush Pervasives.stdout;
  diff >= Config.bored;;
