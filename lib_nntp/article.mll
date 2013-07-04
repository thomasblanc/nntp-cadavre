{
type header = Fu2 of string | Newsgroups of string | Subject of string | References of string | Unknown of string;;
}

rule header = parse
  "Followup-To: " (_* as s) eof { Fu2 s}
| "Newsgroups: " (_* as s) eof { Newsgroups s}
| "Subject: " (_* as s) eof { Subject s}
| "References: " (_* as s) eof { References s}
| _* as s eof { Unknown s}

{
 let parse_header h = header (Lexing.from_string h);;
}
