{
exception Unknown_newsgroup;;
exception No_newsgroup;;
let random_id s =
 "<"^ ( string_of_int ( Random.bits())) ^"$"^ ( string_of_int ( Random.bits ()))^"@"^s^">";;
}

let num = ['0'-'9']*
let group_name = [^'\r'' ''\n']+
let anything = [^'\r''\n']*
let id = '<'([^'>''\n''\r']+)'>'
let sp = ' '
let eol = (sp anything)? eof

rule group_info = parse
  "211" sp (num as number) sp (num as low) sp (num as high) sp (group_name as group) eol { number, low, high, group }
| "411" eol {raise Unknown_newsgroup}
| "412" eol {raise No_newsgroup}
| _* eol { raise (Failure "group")}
and article_info = parse
  "220" sp (num as number) sp (id as id) (sp anything)* eof { number, id}
| _* eof { raise ( Failure "article info")}
and post_lexing serv = parse
  "340" (sp anything)* (id as id) eol {id}
| "340" eol { random_id serv }
| _* eof { raise ( Failure "post lexing")}

{
let parse_listgroup s = group_info (Lexing.from_string s);;
let parse_article s = article_info (Lexing.from_string s);;
let parse_post serv s = post_lexing serv (Lexing.from_string s);;
}
