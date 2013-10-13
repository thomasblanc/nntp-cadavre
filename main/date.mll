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

}

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

rule date = parse
| (wd as wd)", "(md as md)' '(month as mon)' '(year as y)' '(hour as h)':'(min as min)':'(sec as sec)' '(decal_sign as ds) (hour as dh) (min as dm)
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
    let open Unix in
    mktime
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
  }
| _ { raise ( Invalid_argument "date" ) }

{
  let of_string s = date ( Lexing.from_string s )

  let to_string g = 
    let to_wd = function
        0 -> "Sun" | 1 -> "Mon" | 2 -> "Tue" | 3 -> "Wed" | 4 -> "Thu" | 5 -> "Fri" | 6 -> "Sat" | _ -> assert false
    and to_mon = function
        0 -> "Jan" | 1 -> "Feb" | 2 -> "Mar" | 3 -> "Apr" | 4 -> "May" | 5 -> "Jun" | 6 -> "Jul"
      | 7 -> "Aug" | 8 -> "Sep" | 9 -> "Oct" | 10 -> "Nov" | 11 -> "Dec" | _ -> assert false
    in
    let open Unix in
    Printf.sprintf "%s, %0d %s %0d %02d:%02d:%02d +0000 (UTC)" (to_wd g.tm_wday) g.tm_mday (to_mon g.tm_mon) (g.tm_year +1900) g.tm_hour g.tm_min g.tm_sec
}
