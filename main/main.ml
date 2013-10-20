let termination = Sys.Signal_handle ( fun _ -> ignore (Unix.system ("rm -f "^Config.pid_file)); exit 0);;

let _ =
  if (Array.length Sys.argv) > 1 && Sys.argv.(1) = "-f"
  then ()
  else
    begin
      let _ = Unix.system (Printf.sprintf "echo %d > %s\n" (Unix.getpid ()) Config.pid_file) in
      Sys.set_signal Sys.sigint termination;
      Sys.set_signal Sys.sigquit termination;
      Sys.set_signal Sys.sigterm termination;
      Daemon.as_daemon ~keep_dir:true ~keep_fd:true
    end;;

let check () =
 let c = Nntp.connection_as_reader Config.server Config.port in
 let c = let login = Config.login and password = Config.password in
    if login <> "" && password <> "" then Nntp.authentificate c login password else c in
 let wildmat = Wildmat.of_string Config.group and
 tm = snd ( Cadavre.last_time ()) in
 begin
  try
   let l = Nntp.newnews ~wildmat ~tm c in
   List.iter
    (fun id -> Cadavre_parse.parse_article id ( String.concat "\r\n" ( Nntp.article_id c id)) )
    l
  with Failure "newnews not implemented" ->
   let l = List.tl (snd (Nntp.list_group c Config.group ((Cadavre.last_seen_number ())^"-"))) in
   let rec aux l =
   match l with
     number::h::tl ->
      (
       let (id,a) = Nntp.article_number c number in
       Cadavre_parse.parse_article id ( String.concat "\r\n" a);
       aux (h::tl)
      )
   | [number] ->
      (
       let (id,a) = Nntp.article_number c number in
       Cadavre_parse.parse_article id ( String.concat "\r\n" a);
       Cadavre.save_last_number number
      )
    | [] -> ()
    in
    aux l;
  end;
 Nntp.close c;
 if Cadavre.is_bored () then Cadavre.post_sentence ();
 Cadavre.save_data ();;

let keep_on = ref true;;
let keep_mutex = Mutex.create ();;

let rec looping () =
 let t = Unix.time () in
 (* Printf.printf "It is now %f !\n" t; *)
  begin
  try check () with
    Failure "Connecting" | Unix.Unix_error(Unix.ECONNREFUSED,_,_) -> ()
  | _ -> print_endline "Unknown error, ignoring." (*errors are ignored*)
  end;
(*  Mutex.lock keep_mutex;
  if !keep_on then
  begin
   Mutex.unlock keep_mutex;*)
 Unix.sleep ( (int_of_float (t-.(Unix.time () ) ) ) + Config.refresh);
   (*match Unix.select [Unix.stdin] [] [] ( (  ((Unix.time ()) -. t)) +. (float Config.refresh)) with
     ([a],[],[]) -> print_endline "has been stopped by stdin."
   | _ ->*) looping ()
  (*end
  else
   Mutex.unlock keep_mutex*);;
(*
let reading () = let _ = read_line () in Mutex.lock keep_mutex; keep_on := false; Mutex.unlock keep_mutex;;

let t1 = Thread.create looping ()
and _ = Thread.create reading () in
Thread.join t1;;*)

let _ = looping ();;
