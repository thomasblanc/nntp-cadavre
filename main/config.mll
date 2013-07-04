{
exception Eof;;

open Hashtbl;;

let strings = create 15;;
let ints = create 5;;

let file = "/etc/cadavre/cadavre.conf"

}

let ident = ['a'-'z''_']+
let string_val = '"' _* '"'
let b = ['\t'' ']*


let eq = '='

rule parse_conf = parse
  (ident as i) eq (['1'-'9']['0'-'9']* as n) { add ints i (int_of_string n)}
| (ident as i) eq '"' ( [^'\n''"']* as s) '"' { add strings i s}
| (ident as i) eq "<<" (([^'>']*|'>'[^'>'])* as s) ">>"  { add strings i s}
| [^'=''\n']* '\n' { parse_conf lexbuf }
| [^'=''\n']* eof { raise Eof}

{
let i = open_in file;;
let lexbuf = Lexing.from_channel i;;

let _ =
 try
  let rec aux () = parse_conf lexbuf; aux () in aux ()
 with
  Eof -> ()

(* main configuration *)
let s i = try find strings i with _ -> raise (Failure i);;
let i s = try find ints s with _ -> raise (Failure s);;

let server = s "server";;
let port = i "port"
let group = s "newsgroup";;
let refresh = i "refresh_time";;
let bored = float_of_int ( i "boring_time");;
let from = s "email";;
let data = s "data";;
let login = try s "login" with _ -> "";;
let password = try s "password" with _ -> "";;
let pid_file = s "pid_file";;
(* message generating *)
let subject = s "subject";;
let msg_begin = s "msg_begin";;
let msg_end = s "msg_end";; 
let signature = s "sig";;

(* misc *)
let help_msg = s "help_msg";;
}
