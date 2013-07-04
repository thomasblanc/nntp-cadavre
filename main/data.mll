{
type t =
 {
  mutable last_post : int;
  mutable last_seen_number : Nntp.number;
  mutable last_time : float * Unix.tm;
  mutable subjects : ( string * int ) array;
  mutable verbs : (string array array) array;
  mutable comps : string array;
  mutable dates : ( string * int ) array;
  mutable places : string array;
 }

let get_data file =
 let f = open_in_bin file in
 let d = Marshal.from_channel f in
 close_in f; d
let save_data d file =
 let f = open_out_bin file in
 Marshal.to_channel f d [];
 close_out f;;

let ask_last_number () =
 let c = Nntp.connection_as_reader Config.server Config.port in
 let c =
   let l = Config.login and p = Config.password in
   if l <> "" && p <> "" then Nntp.authentificate c l p else c
 in
 let (_,_,h,_) = Nntp.group c Config.group in
 Nntp.close c; h;;

}

let endl = '\r'?'\n'
let word = [^'\r''\n']+
let number = ['0'-'9']+

rule subject = parse
  word as w endl (number as n) { (w, pred ( int_of_string n)) }
| _|eof { raise ( Failure "subject")}

and verb = parse
  (word as w1) endl (word as w2) endl (word as w3) endl (word as w4) endl (word as w5) endl (word as w6) endl
  (word as x1) endl (word as x2) endl (word as x3) endl (word as x4) endl (word as x5) endl (word as x6) endl
  (word as y1) endl (word as y2) endl (word as y3) endl (word as y4) endl (word as y5) endl (word as y6) endl
  (word as z1) endl (word as z2) endl (word as z3) endl (word as z4) endl (word as z5) endl (word as z6)
   {
    [|
     [|w1;w2;w3;w4;w5;w6|];
     [|x1;x2;x3;x4;x5;x6|];
     [|y1;y2;y3;y4;y5;y6|];
     [|z1;z2;z3;z4;z5;z6|]
    |]
   }
| _|eof { raise ( Failure "verb")}

and comp = parse
 word as word { word }
| eof { ""}
 
and options  = parse
(word endl)* "msgid=" (number as n) { int_of_string n}
| _|eof { raise ( Failure "options")}


{
let of_old_cadavre dir =
 let get_list_of_words w =
  let r = Sys.readdir (Filename.concat dir w) in
  Array.fast_sort (fun x y -> compare (int_of_string x) (int_of_string y)) r; r in
 let get_list w p =
  Array.map
   ( fun file ->
     let f = open_in ( Filename.concat ( Filename.concat dir w) file) in
     let r = p ( Lexing.from_channel f) in
     close_in f; r )
   ( get_list_of_words w)
 in
 let r =
 {
  last_post = ( let f = open_in (Filename.concat dir "config") in let r = options (Lexing.from_channel f) in close_in f;r);
  last_seen_number = ask_last_number ();
  last_time = ( let t = Unix.time () in (t, Unix.gmtime t));
  subjects = get_list "sujet" subject;
  verbs = get_list "verbe" verb;
  comps = get_list "complement" comp;
  dates = get_list "date" subject;
  places = get_list "lieu" comp;
 } in
 
 (*Array.iter (fun v -> Array.iter (fun t ->print_endline (String.concat " " (Array.to_list t))) v) r.verbs;*)

 r
 ;;

}
