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

let re_url_encoding = Str.regexp "%\\([0-9A-Fa-f][0-9A-Fa-f]\\)"

let decode_url_percent s =
  let n = int_of_string ("0x" ^ Str.matched_group 1 s) in
  String.make 1 (Char.chr n)

let decode_url s =
  Str.global_substitute re_url_encoding decode_url_percent s

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

  let create() = { txt = Buffer.create 2048; extra = Buffer.create 256 }

  let clear ob =
    Buffer.clear ob.txt;
    Buffer.clear ob.extra

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
          "href" -> add_extra ob (decode_url s)
        | _ -> ()
        end
    | "img" ->
        begin match String.lowercase n with
          "src" -> add_extra ob (decode_url s)
        | "alt" -> add_extra ob s
        | _ -> ()
        end
    | "font" ->
        begin match String.lowercase n with
          "face" | "color" -> add_extra ob s
        | _ -> ()
        end
    | _ -> ()     
end

let ob = Output.create()
let tag = ref ""
let attr_name = ref ""
let attr_value = Buffer.create 128

}

let ws = [' ' '\n' '\r' '\t']
let name = ['A'-'Z' 'a'-'z'] ['A'-'Z' 'a'-'z' '0'-'9' '.' '-']*
let unquotedattrib = 
  [^ '\'' '\"' '>' ' ' '\n' '\r' '\t'] [^ '>' ' ' '\n' '\r' '\t']*

rule main = parse
    "<!" ['D' 'd']['O' 'o']['C' 'c']['T' 't']['Y' 'y']['P' 'p']['E' 'e']
         [^ '>'] * '>'
      { main lexbuf }
  | "<!--"
      { comment lexbuf; main lexbuf }
  | "<" name
      { let s = Lexing.lexeme lexbuf in
        tag := String.lowercase(String.sub s 1 (String.length s - 1));
        tagbody lexbuf;
        main lexbuf }
  | "</" name
      { let s = Lexing.lexeme lexbuf in
        tag := String.lowercase(String.sub s 2 (String.length s - 2));
        tagbody lexbuf;
        main lexbuf }
  | "<"                         (* tolerance *)
      { Output.char ob '<'; main lexbuf }
  | "&"
      { Output.char ob (entity lexbuf); main lexbuf }
  | [^ '<' '&']+
      { Output.string ob (Lexing.lexeme lexbuf); main lexbuf }
  | eof
      { Output.contents ob }

and comment = parse
    "--" ws* ">"
      { () }
  | _                           (* tolerance *)
      { comment lexbuf }
  | eof                         (* tolerance *)
      { () }

and tagbody = parse
    ">"
      { Output.tag ob !tag }
  | name
      { attr_name := Lexing.lexeme lexbuf;
        tagattrib lexbuf;
        tagbody lexbuf }
  | _                           (* tolerance -- should be ws *)
      { tagbody lexbuf }
  | eof                         (* tolerance *)
      { Output.tag ob !tag }

and tagattrib = parse
    ws* '=' ws* 
      { tagvalue lexbuf }
  | ""
      { Output.tag_attr ob !tag !attr_name "" }

and tagvalue = parse
    "'"
      { Buffer.clear attr_value; singlequoted lexbuf }
  | "\""
      { Buffer.clear attr_value; doublequoted lexbuf }
  | unquotedattrib
      { Output.tag_attr ob !tag !attr_name (Lexing.lexeme lexbuf) }
  | ""                          (* tolerance *)
      { Output.tag_attr ob !tag !attr_name "" }

and singlequoted = parse
    "'" | eof                   (* eof is tolerance *)
      { Output.tag_attr ob !tag !attr_name (Buffer.contents attr_value) }
  | "&"
      { Buffer.add_char attr_value (entity lexbuf); singlequoted lexbuf }
  | _
      { Buffer.add_char attr_value (Lexing.lexeme_char lexbuf 0);
        singlequoted lexbuf }

and doublequoted = parse
    "\"" | eof                 (* eof is tolerance *)
      { Output.tag_attr ob !tag !attr_name (Buffer.contents attr_value) }
  | "&"
      { Buffer.add_char attr_value (entity lexbuf); doublequoted lexbuf }
  | _
      { Buffer.add_char attr_value (Lexing.lexeme_char lexbuf 0);
        doublequoted lexbuf }

and entity = parse
    '#' ['0'-'9']+
      { let s = Lexing.lexeme lexbuf in
        let n = int_of_string (String.sub s 1 (String.length s - 1)) in
        entity_end lexbuf;
        if n >= 0 && n <= 255 then Char.chr n else '\255' }
  | name
      { let s = Lexing.lexeme lexbuf in
        entity_end lexbuf;
        try StringMap.find s entity_table with Not_found -> '\255' }
  | ""                          (* tolerance *)
      { '&' }

and entity_end = parse
    ";" ?
      { () }

{
let extract_text s =
  Output.clear ob; main (Lexing.from_string s)
}
