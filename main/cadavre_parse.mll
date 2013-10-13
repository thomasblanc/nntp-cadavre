{

let from_mon m = ( match m with "Jan" -> 1 | "Feb" -> 2 | "Mar" -> 3 | "Apr" -> 4 | "May" -> 5 | "Jun" -> 6 | "Jul"
 -> 7 | "Aug" -> 8 | "Sep" -> 9 | "Oct" -> 10 | "Nov" -> 11 | "Dec" -> 12 | _ -> raise (Failure "Month") ) - 1;;
let from_weekd = function
 "Sun" -> 0 | "Mon" -> 1 | "Tue" -> 2 | "Wed" -> 3 | "Thu" -> 4 | "Fri" -> 5 | "Sat" -> 6 | _ -> raise (Failure "Weekday")

let decal_hour = (* FUCKING HACK ! *)
  let t = Unix.time () in
  let l = Unix.localtime t and g = Unix.gmtime t in
  let l = int_of_float (fst (Unix.mktime l))
  and g = int_of_float (fst (Unix.mktime g)) in
  l-g;;

let gen_table root group bottom =
  Array.map (Array.map (fun (pre,term) -> Printf.sprintf "%s%s%s%s" pre root term bottom))
    (
      match group with
	"er" ->
	  [|
	    [|"","e";"","es";"","e";"","ons";"","ez";"","ent" |];
	    [|"","ais";"","ais";"","ait";"","ions";"","iez";"","aient"|];
	    [|"","erai";"","eras";"","era";"","erons";"","erez";"","eront"|];
	    [|"ai ","é";"as ","é";"a ","é";"avons ","é";"avez ","é";"ont ","é"|]
	  |]
      | "ir" ->
	[|
	  [|"","is";"","is";"","it";"","issons";"","issez";"","issent" |];
	  [|"","issais";"","issais";"","issait";"","issions";"","issiez";"","issaient"|];
	  [|"","irai";"","iras";"","irai";"","irons";"","irez";"","iront"|];
	  [|"ai ","i";"as ","i";"a ","i";"avons ","i";"avez ","i";"ont ","i"|]
	|]
      | _ -> raise Not_found
    );;

open Unix;;

}
let b = [' ''\t']+
let endl = "\r\n"
let any = [^'\r']
let anys = any*
let ignoring = anys endl

let help_sentence = "Le cadavre, c'est quoi donc ?"
let teach_sentence = "J'enseigne au cadavre"
let subject = "sujet"
let verb = "verbe"
let comp = "complement"
let date = "date"
let place = "lieu"

let cy = ['0'-'9']
let wd = "Mon"|"Tue"|"Sun"|"Wed"|"Thu"|"Fri"|"Sat"
let md = ['0'-'3']? cy
let month = "Jan"|"Feb"|"Mar"|"Apr"|"May"|"Jun"|"Jul"|"Aug"|"Sep"|"Oct"|"Nov"|"Dec"
let year = ( cy cy)? cy cy
let hour = cy? cy
let min = cy? cy
let sec = cy? cy
let decal_sign = ['-''+']
let decal = cy cy cy cy
let id = '<'[^'>']+'>'

let v = [^'$''\r''\n']*
let v_s = ("er"|"ir")




rule parse_headers id cont = parse
  "Date: "(wd as wd)", "(md as md)' '(month as mon)' '(year as y)' '(hour as h)':'(min as min)':'(sec as sec)' '(decal_sign as ds) (hour as dh) (min as dm) ignoring
  {
   let i = int_of_string in
   let dm = i dm and dh = i dh in
   let (dm,dh) = if ds = '-' then (~-dm,~-dh) else (dm,dh) in
   let y =
   if String.length y = 4
    then ((i y)-1900)
    else
     let year = i y  and this_y = (Unix.localtime (Unix.time ())).Unix.tm_year in
     let ty_e =  this_y mod 100 in
     (if year <= ty_e then this_y-ty_e+year else this_y-ty_e+year-100)
    in
   Cadavre.add_time
    ( mktime
     {
      tm_sec = (i sec)+decal_hour;
      tm_min = (i min) - dm;
      tm_hour = (i h) - dh;
      tm_mday = i md;
      tm_mon = from_mon mon;
      tm_year = y;
      tm_wday = from_weekd wd;
      tm_yday = 0;
      tm_isdst = false
     }
    );
   parse_headers id cont lexbuf}
| "From: " (anys as f) endl {  parse_headers id (f <> Config.from) lexbuf}
| any ignoring { parse_headers id cont lexbuf}
| endl {if cont then parse_body id lexbuf else print_endline "skipping self entry" }

and parse_body id= parse
  help_sentence ignoring {Cadavre.send_help id; parse_body id lexbuf}
| teach_sentence endl {parse_teach id lexbuf}
| ignoring {parse_body id lexbuf}
| help_sentence eof {Cadavre.send_help id}
| anys eof {()}

and parse_teach id = parse
  subject endl { parse_subject id lexbuf}
| verb endl { parse_verb id lexbuf}
| comp endl { parse_comp id lexbuf}
| date endl { parse_date id lexbuf}
| place endl {parse_place id lexbuf}
| _ {parse_body id lexbuf}

and parse_subject id = parse
  ((any+) as subj) endl (['1'-'6'] as according) endl { Cadavre.add_subject ~id subj ((int_of_char according)-(int_of_char '0')); parse_body id lexbuf}
| _ {parse_body id lexbuf}

and parse_verb id = parse
(any+ as pres1) endl
(any+ as pres2) endl
(any+ as pres3) endl
(any+ as pres4) endl
(any+ as pres5) endl
(any+ as pres6) endl
(any+ as past1) endl
(any+ as past2) endl
(any+ as past3) endl
(any+ as past4) endl
(any+ as past5) endl
(any+ as past6) endl
(any+ as future1) endl
(any+ as future2) endl
(any+ as future3) endl
(any+ as future4) endl
(any+ as future5) endl
(any+ as future6) endl
(any+ as pres_past1) endl
(any+ as pres_past2) endl
(any+ as pres_past3) endl
(any+ as pres_past4) endl
(any+ as pres_past5) endl
(any+ as pres_past6) endl
 { Cadavre.add_verb ~id
 [|
  [|pres1;pres2;pres3;pres4;pres5;pres6|];
  [|past1;past2;past3;past4;past5;past6|];
  [|future1;future2;future3;future4;future5;future6|];
  [|pres_past1;pres_past2;pres_past3;pres_past4;pres_past5;pres_past6|]
 |]; parse_body id lexbuf}
| (v as root) '$'(v_s as group)'$' (v as bottom) endl
     { Cadavre.add_verb ~id
       (gen_table root group bottom); parse_body id lexbuf}
| _ {parse_body id lexbuf}

and parse_comp id = parse
  (any+ as comp) endl { Cadavre.add_comp ~id comp;parse_body id lexbuf}
| _ {parse_body id lexbuf}

and parse_date id = parse
  (any+ as d) endl (['1'-'4'] as t) endl { Cadavre.add_date ~id d ((int_of_char t)-(int_of_char '0'));parse_body id lexbuf}
| _ {parse_body id lexbuf}

and parse_place id = parse
  (any+ as place) endl { Cadavre.add_place ~id place;parse_body id lexbuf}
| _ {parse_body id lexbuf}

{
let parse_article id a =
 parse_headers id true (Lexing.from_string a)
}
