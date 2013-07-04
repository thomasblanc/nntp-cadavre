open Unix;;

let endl = "\r\n";;

let write o s =
(* print_string s ; flush Pervasives.stdout;*)
 output_string o s;;
let read_line i =
 let s = input_line i in
 (*print_endline s;*)  String.sub s 0 ((String.length s)-1);;

let from_gmtime g =
 let to_wd = function
   0 -> "Sun" | 1 -> "Mon" | 2 -> "Tue" | 3 -> "Wed" | 4 -> "Thu" | 5 -> "Fri" | 6 -> "Sat" | _ -> raise (Failure "Weekday")
 and to_mon = function
   0 -> "Jan" | 1 -> "Feb" | 2 -> "Mar" | 3 -> "Apr" | 4 -> "May" | 5 -> "Jun" | 6 -> "Jul"
 | 7 -> "Aug" | 8 -> "Sep" | 9 -> "Oct" | 10 -> "Nov" | 11 -> "Dec" | _ -> raise (Failure "Month")
 in
 Printf.sprintf "%s, %0d %s %0d %02d:%02d:%02d +0000 (UTC)" (to_wd g.tm_wday) g.tm_mday (to_mon g.tm_mon) (g.tm_year +1900) g.tm_hour g.tm_min g.tm_sec;;

let check_code code s =
 s.[0] = code.[0] && s.[1] = code.[1] && s.[2] = code.[2];;

let has_mode cap_list mode =
 ( List.exists (fun l ->  ( String.uppercase (fst l) = mode)) cap_list);;
let has_mode_arg cap_list mode arg =
 List.exists
  (
   fun l ->
    if ( String.uppercase (fst l) = mode)
    then List.exists ( fun w ->  ( ( String.uppercase w) = arg)) (snd l)
    else  false
  )
  cap_list;;

let dot_stuff s =
 let b = Buffer.create (String.length s) in
 let rec aux i j =
 try let k = String.index_from s j '\n' in
 if s.[succ k] = '.' then
 (
  Buffer.add_substring b s i (k-i+2);
  Buffer.add_char b '.';
  aux (k+2) (k+2)
 )
 else
  aux i (succ k)
  with _ -> Buffer.add_substring b s i ((String.length s)-i)
 in aux 0 0;
 Buffer.contents b
 ;;

(* cut a string into substrings using character c as separator *)
let string_cut c s =
 let rec aux i res =
  try
   let j = String.rindex_from s (i-1) c in
   aux j ((String.sub s (succ j) (i-j-1))::res)
  with _ -> ((String.sub s 0 i)::res)
 in aux (String.length s) []
;;

let get_multiline inp =
 let rec aux res =
  match read_line inp with
    "." ->  res
  | s -> aux ( (s)::res)
 in List.rev (aux []);;

