(***********************************************************************)
(*                                                                     *)
(*                 SpamOracle -- a Bayesian spam filter                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  This file is distributed under the terms of the   *)
(*  GNU Public License version 2, http://www.gnu.org/licenses/gpl.txt  *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(** Approximate HTML scanner.  Extracts words from HTML text,
    as well as certain parameters of certain tags (e.g. URLs). *)

{
module StringSet = Set.Make(String)
module StringMap = Map.Make(String)

let dec_attr = ref ((fun _ -> assert false) : string -> string)
let dec_url = ref ((fun _ -> assert false) : string -> string)

let entity_table =
  List.fold_left
    (fun t (s, c) -> StringMap.add s c t)
    StringMap.empty
    ["amp", '&';          "lt", '<';           "gt", '>';
     "nbsp", '\160';     
     "Agrave", '\192';    "Aacute", '\193';    "Acirc", '\194';
     "Atilde", '\195';    "Auml", '\196';      "Aring", '\197';
     "AElig", '\198';     "Ccedil", '\199';    "Egrave", '\200';
     "Eacute", '\201';    "Ecirc", '\202';     "Euml", '\203';
     "Igrave", '\204';    "Iacute", '\205';    "Icirc", '\206';
     "Iuml", '\207';      "ETH", '\208';       "Ntilde", '\209';
     "Ograve", '\210';    "Oacute", '\211';    "Ocirc", '\212';
     "Otilde", '\213';    "Ouml", '\214';      "times", '\215';
     "Oslash", '\216';    "Ugrave", '\217';    "Uacute", '\218';
     "Ucirc", '\219';     "Uuml", '\220';      "Yacute", '\221';
     "THORN", '\222';     "szlig", '\223';     "agrave", '\224';
     "aacute", '\225';    "acirc", '\226';     "atilde", '\227';
     "auml", '\228';      "aring", '\229';     "aelig", '\230';
     "ccedil", '\231';    "egrave", '\232';    "eacute", '\233';
     "ecirc", '\234';     "euml", '\235';      "igrave", '\236';
     "iacute", '\237';    "icirc", '\238';     "iuml", '\239';
     "eth", '\240';       "ntilde", '\241';    "ograve", '\242';
     "oacute", '\243';    "ocirc", '\244';     "otilde", '\245';
     "ouml", '\246';      "divide", '\247';    "oslash", '\248';
     "ugrave", '\249';    "uacute", '\250';    "ucirc", '\251';
     "uuml", '\252';      "yacute", '\253';    "thorn", '\254';
     "yuml", '\255']    

let word_breaking_tags =
  List.fold_right StringSet.add
    [ "p"; "br"; "ul"; "ol"; "dt"; "li"; "dd"; "table"; "tr"; "th"; "td";
      "img"; "div"; "blockquote"; "h1"; "h2"; "h3"; "h4"; "h5"; "h6";
      "address" ]
    StringSet.empty

module Output = struct

  type t = { txt: Buffer.t; extra: Buffer.t }

  let create() = { txt = Buffer.create 16384; extra = Buffer.create 1024 }

  let contents ob =
    Buffer.add_char ob.txt '\n';
    Buffer.add_buffer ob.txt ob.extra;
    Buffer.contents ob.txt

  let char ob c =
    Buffer.add_char ob.txt c

  let string ob s =
    Buffer.add_string ob.txt s

  let tag ob t =
    if StringSet.mem t word_breaking_tags then char ob ' '

  let add_extra ob s =
    Buffer.add_string ob.extra s; Buffer.add_char ob.extra '\n'

  let tag_attr ob t n s =
    match t with
      "a" ->
        begin match String.lowercase n with
          "href" -> add_extra ob (!dec_url (!dec_attr s))
        | _ -> ()
        end
    | "img" ->
        begin match String.lowercase n with
          "src" -> add_extra ob (!dec_url (!dec_attr s))
        | "alt" -> add_extra ob (!dec_attr s)
        | _ -> ()
        end
    | _ -> ()     
end
}

let ws = [' ' '\n' '\r' '\t']
let name = ['A'-'Z' 'a'-'z'] ['A'-'Z' 'a'-'z' '0'-'9' '.' '-']*
let unquotedattrib = [^ '>' ' ' '\n' '\r' '\t']+
let hexdigit = ['0'-'9' 'A'-'F' 'a'-'f']

rule main ob = parse
    "<!" ['D' 'd']['O' 'o']['C' 'c']['T' 't']['Y' 'y']['P' 'p']['E' 'e']
         [^ '>'] * '>'
      { main ob lexbuf }
  | "<!--"
      { comment lexbuf; main ob lexbuf }
  | "<" "/"? (name as tag)
      { tagbody ob (String.lowercase tag) lexbuf;
        main ob lexbuf }
  | "<"                         (* tolerance *)
      { Output.char ob '<'; main ob lexbuf }
  | "&"
      { Output.char ob (entity lexbuf); main ob lexbuf }
  | [^ '<' '&']+
      { Output.string ob (Lexing.lexeme lexbuf); main ob lexbuf }
  | eof
      { Output.contents ob }

and comment = parse
    "--" ws* ">"
      { () }
  | _                           (* tolerance *)
      { comment lexbuf }
  | eof                         (* tolerance *)
      { () }

and tagbody ob tag = parse
    ">"
      { Output.tag ob tag }
  | (name as n) ws* '=' ws* "'" ([^ '\'']* as s) "'"
      { Output.tag_attr ob tag n s; tagbody ob tag lexbuf }
  | (name as n) ws* '=' ws* "\"" ([^ '\"']* as s) "\""
      { Output.tag_attr ob tag n s; tagbody ob tag lexbuf }
  | (name as n) ws* '=' ws* (unquotedattrib as s)
      { Output.tag_attr ob tag n s; tagbody ob tag lexbuf }
  | name as n
      { Output.tag_attr ob tag n ""; tagbody ob tag lexbuf }
  | _                           (* tolerance -- should be ws *)
      { tagbody ob tag lexbuf }
  | eof                         (* tolerance *)
      { Output.tag ob tag }

and entity = parse
    '#' (['0'-'9']+ as s) ';'?
      { let n = int_of_string s in
        if n >= 0 && n <= 255 then Char.chr n else '\255' }
  | (name as s) ';'?
      { try StringMap.find s entity_table with Not_found -> '\255' }
  | ""                          (* tolerance *)
      { '&' }

and decode_attribute ob = parse
    '&'
      { Buffer.add_char ob (entity lexbuf); decode_attribute ob lexbuf }
  | [^ '&']+ as s
      { Buffer.add_string ob s; decode_attribute ob lexbuf }
  | eof
      { Buffer.contents ob }

and decode_url ob = parse
    '%' (hexdigit hexdigit as s)
      { let n = int_of_string ("0x" ^ s) in
        Buffer.add_char ob (Char.chr n);
        decode_url ob lexbuf }
  | '%'                         (* tolerance *)
      { Buffer.add_char ob '%'; decode_url ob lexbuf }
  | [^ '%']+ as s
      { Buffer.add_string ob s; decode_url ob lexbuf }
  | eof
      { Buffer.contents ob }

{
let _ =
  dec_attr :=
    (fun s -> decode_attribute (Buffer.create 128) (Lexing.from_string s));
  dec_url :=
    (fun s -> decode_url (Buffer.create 128) (Lexing.from_string s))

let extract_text s =
  main (Output.create()) (Lexing.from_string s)
}
