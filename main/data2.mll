{
open Unix
open Data

let i = int_of_string

}

let endl = '\r'? '\n'
let eq = '='
let num = ['1'-'9'] ['0'-'9']*
let lp = "last_post"
let li = "last_id"
let lt = "last_time"
let any = [^ '\n' ]*
let ef = endl* eof


rule conffile lastp lastid lastt = parse
| lp eq ( num as n ) endl
    { conffile (i n) lastid lastt lexbuf }
| li eq ( any as s ) endl
    { conffile lastp s lastid lastt lexbuf }
| lt eq any endl
    { conffile lastp lastid (Date.of_string s) lexbuf }
| eof { (lastp, lastid, lastt) }
| _ { raise Invalid_arg "data file" }

rule subject = parse
| ( any as s ) endl
  ( num as n ) ef
  { s, pred (i n) }
| _ { raise ( Invalid_arg "subject file" ) }

rule verb = parse
| (any as w1) endl (any as w2) endl (any as w3) endl (any as w4) endl (any as w5) endl (any as w6) endl
  (any as x1) endl (any as x2) endl (any as x3) endl (any as x4) endl (any as x5) endl (any as x6) endl
  (any as y1) endl (any as y2) endl (any as y3) endl (any as y4) endl (any as y5) endl (any as y6) endl
  (any as z1) endl (any as z2) endl (any as z3) endl (any as z4) endl (any as z5) endl (any as z6) ef
   {
    [|
     [|w1;w2;w3;w4;w5;w6|];
     [|x1;x2;x3;x4;x5;x6|];
     [|y1;y2;y3;y4;y5;y6|];
     [|z1;z2;z3;z4;z5;z6|]
    |]
   }
| _ { raise ( Invalid_arg "verb file" )

rule comp_or_place = parse
| ( any as s ) ef
    { s }
| _ { raise ( Invalid_arg "comp or place file" )

rule date = parse
| ( any as s ) endl ( num as n ) ef
    { s, pred ( i n ) }
| _ { raise ( Invalid_arg "date file" ) }

{

  let get_data dir =
    let data = ref
        {
          last_post = 0;
          last_seen_number = "";
          last_time = mktime ( time () );
          subjects = [||];
          verbs = [||];
          comps = [||];
          dates = [||];
          places = [||];
        } in
    let d = opendir dir in

    let files fl =
      let a = Sys.readdir fl in
      Array.fast_sort (fun a b -> compare (i a) (i b) ) a;
      a
    in

    let olex lexer fl =
      lexer ( Lexing.from_channel ( Pervasives.open_in fl ) )
    in
    let alex lexer fl =
      Array.map ( olex lexer ) ( files fl )
    in

    let rec aux () =
      let f = readdir d in
      let fl = Filename.concat dir f in
      match f with
      | "settings" ->
        let (lp, li, lt) =
          olex
            (conffile lastp lastid lastt lb)
            fl
        in
        data :=
          { !data with
            last_post = lp;
            last_seen_number = li;
            last_time = lt; };
        aux ()
      | "subjects" ->
        data := {!data with subjects = alex subject fl; };
        aux ()
      | "verbs" ->
        data := {!data with verbs = alex verb fl; };
        aux ()
      | "comps" ->
        data := {!data with comps = alex comp_or_place fl; };
        aux ()
      | "dates" ->
        data := {!data with dates = alex date fl; };
        aux ()
      | "places" ->
        data := {!data with places = alex comp_or_place fl; };
        aux ()

    in
    try aux () with Not_found -> !data
      
let save_data da dir =
  let open Pervasives in
  let si = string_of_int in
  let file f = Filename.concat dir f in
  let settings = open_out (file "settings") in
  Printf.fprintf settings
    "last_post=%d\nlast_id=%s\nlast_time=%s"
    da.last_post da.last_seen_number
    ( Date.to_string (snd da.last_time) );
  close_out settings;
  let file f num = Filename.concat ( file f ) ( si num ) in
  Array.iteri (fun n (s,i) ->
      let sf = open_out (file "subject" n) in
      Printf.fprintf sf "%s\n%d" s (si (succ i));
      close_out sf)
    da.subject;
  Array.iteri (fun n a ->
      let vf = open_out (file "verbs" n) in
      Array.iter ( Array.iter ( Printf.fprintf vf "%s\n" ) ) a
      close_out vf)
    da.verbs;
  Array.iteri (fun n s ->
      let cf = open_out (file "comps" n) in
      output_string cf s;
      close_out cf)
    da.comps;
  Array.iteri (fun n (s,i) ->
      let sf = open_out (file "dates" n) in
      Printf.fprintf sf "%s\n%d" s (si (succ i));
      close_out sf)
    da.dates;
  Array.iteri (fun n s ->
      let pf = open_out (file "places" n) in
      output_string pf s;
      close_out pf)
    da.places
  
}
