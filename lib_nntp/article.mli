type header = Fu2 of string | Newsgroups of string | Subject of string | References of string | Unknown of string

val parse_header : string -> header
