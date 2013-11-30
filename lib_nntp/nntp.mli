type t;;
type id = string;;
type number = string;;
type article = string list;;

val mime_headers_text : encoding:string -> string list
val connection : string -> int -> t
val connection_as_reader : string -> int -> t
val authentificate : t -> string -> string -> unit
val group : t -> string -> ( number * string * string * string )
val list_group : t -> string -> string -> ( number * string * string * string ) * ( string list)
val close : t -> unit
val article_number : t -> number -> id * article
val article_id : t -> id -> article
val post : from:string -> groups:(string list) -> subject:string -> ?date:Unix.tm
 -> ?additionnal_headers:string list -> body:string -> server_name:string -> t -> unit
val post_reply : id:id -> from:string -> ?groups:(string list) -> ?subject:string -> ?date:Unix.tm
 -> ?additionnal_headers:string list -> body:string -> server_name:string -> t -> unit
val newnews : wildmat:Wildmat.t -> tm:Unix.tm -> t -> id list
