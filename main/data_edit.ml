open Data

let d = get_data Config.data

let display s =
match s with
  "sujet" ->
    Array.iteri (fun id (s,i) -> Printf.printf "%d: %s (%d)\n" id s i) d.subjects
| "verbe" ->
  Array.iteri (fun id a -> Printf.printf "%d: %s\n" id a.(0).(0)) d.verbs
| "complement" ->
  Array.iteri (fun id s -> Printf.printf "%d: %s\n" id s) d.comps
| "date" ->
  Array.iteri (fun id (s,i) -> Printf.printf "%d: %s (%d)\n" id s i) d.dates
| "lieu" ->
  Array.iteri (fun id s -> Printf.printf "%d: %s\n" id s) d.places
| _ -> print_endline "bad input"

let del_aux a i =
  if i >= Array.length a || i < 0
  then (print_endline "bad id"; a)
  else Array.append (Array.sub a 0 i) (Array.sub a (i+1) ((Array.length a )-i-1))

let del typ id =
  match typ with
    "sujet" ->d.subjects <- del_aux d.subjects id
  | "verbe" -> d.verbs <- del_aux d.verbs id
  | "complement" -> d.comps <- del_aux d.comps id
  | "date" -> d.dates <- del_aux d.dates id
  | "lieu" -> d.places <- del_aux d.places id
  | _ -> print_endline "bad input"

let replace typ id =
  match typ with
  | "sujet" ->
    if id >= Array.length d.subjects || id < 0
    then print_endline "bad id"
    else
      begin
	print_endline "ecrivez le sujet puis la personne sur la ligne d'après";
	let s = read_line () in
	let p = read_int () in
	d.subjects.(id) <- (s,p)
      end
  | "verbe" ->
    if id >= Array.length d.verbs || id < 0
    then print_endline "bad id"
    else
      begin
	print_endline "present imparfait futur passé composé (utf8!)";
	Array.iteri
	  (fun i -> Array.iteri (fun j _ -> d.verbs.(id).(i).(j) <- read_line ()))
	  d.verbs.(id)
      end
  | "complement" ->
    if id >= Array.length d.comps || id < 0
    then print_endline "bad id"
    else
      begin
	print_endline "ecrivez le complement:";
	let s = read_line () in
	d.comps.(id) <- s
      end
  | "date" ->
    if id >= Array.length d.dates || id < 0
    then print_endline "bad id"
    else
      begin
	print_endline "ecrivez le mot puis le temps sur la ligne d'après";
	let s = read_line () in
	let p = read_int () in
	d.dates.(id) <- (s,p)
      end
  | "lieu" ->
    if id >= Array.length d.places || id < 0
    then print_endline "bad id"
    else
      begin
	print_endline "ecrivez le lieu:";
	let s = read_line () in
	d.places.(id) <- s
      end
  | _ -> print_endline "bad input"

let help_msg () = 
  begin
    print_endline "Mauvaise syntaxe ! Il faut faire display, del ou replace.";
    print_endline "Le second argument est forcement sujet, verbe, complement, date ou lieu.";
    print_endline "Pour del et replace, il faut indiquer l'id en troisième argument";
    print_endline "N'oubliez pas de vérifier que vous êtes en utf8";
  end

let _ =
  let a = Sys.argv in
  let argc = Array.length a in
  if argc > 1 then
    begin
      (match a.(1) with
	"display" when argc > 2 -> display a.(2)
      | "del" when argc > 3 -> del a.(2) (int_of_string a.(3))
      | "replace" when argc > 3 -> replace a.(2) (int_of_string a.(3))
      | _ -> help_msg ());
      save_data d Config.data
    end
  else help_msg ()
