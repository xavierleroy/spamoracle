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

(** Parsing of e-mail messages, including attachments *)

type message =
  { headers: (string * string) list;
    body: string;
    parts: message list }

let base64_decode_char c =
  match c with
    'A' .. 'Z' -> Char.code c - 65
  | 'a' .. 'z' -> Char.code c - 97 + 26
  | '0' .. '9' -> Char.code c - 48 + 52
  | '+' -> 62
  | '/' -> 63
  | _ -> -1

let decode_base64 s =
  let d = Buffer.create (String.length s * 3 / 4) in
  let buf = Array.make 4 0 in
  let pos = ref 0 in
  for i = 0 to String.length s - 1 do
    let n = base64_decode_char s.[i] in
    if n >= 0 then begin
      buf.(!pos) <- n;
      incr pos;
      if !pos = 4 then begin
        Buffer.add_char d (Char.chr(buf.(0) lsl 2 + buf.(1) lsr 4));
        Buffer.add_char d (Char.chr((buf.(1) land 15) lsl 4 + buf.(2) lsr 2));
        Buffer.add_char d (Char.chr((buf.(2) land 3) lsl 6 + buf.(3)));
        pos := 0
      end
    end
  done;
  begin match !pos with
    2 ->
      Buffer.add_char d (Char.chr(buf.(0) lsl 2 + buf.(1) lsr 4))
  | 3 ->
      Buffer.add_char d (Char.chr(buf.(0) lsl 2 + buf.(1) lsr 4));
      Buffer.add_char d (Char.chr((buf.(1) land 15) lsl 4 + buf.(2) lsr 2))
  | _ ->
      ()
  end;
  Buffer.contents d

let hexa_digit c =
  if c >= '0' && c <= '9' then Char.code c - 48
  else if c >= 'A' && c <= 'F' then Char.code c - 65 + 10
  else if c >= 'a' && c <= 'f' then Char.code c - 97 + 10
  else raise Not_found

let decode_qp s =
  let len = String.length s in
  let d = Buffer.create (String.length s) in
  let pos = ref 0 in
  while !pos < len do
    let c = s.[!pos] in
    if c = '=' && !pos + 1 < len && s.[!pos + 1] = '\n' then begin
      pos := !pos + 2
    end else if c = '=' && !pos + 2 < len then begin
      try
        let h1 = hexa_digit s.[!pos + 1]
        and h2 = hexa_digit s.[!pos + 2] in
        Buffer.add_char d (Char.chr(h1 lsl 4 + h2));
        pos := !pos + 3
      with Not_found ->
        Buffer.add_char d c;
        incr pos
    end else begin
      Buffer.add_char d c;
      incr pos
    end
  done;
  Buffer.contents d

let re_base64 = Str.regexp_case_fold "base64"
let re_qp = Str.regexp_case_fold "quoted-printable"

let decode encoding s =
  if Str.string_match re_base64 encoding 0 then
    decode_base64 s
  else if Str.string_match re_qp encoding 0 then
    decode_qp s
  else
    s

let re_encoded_header =
  Str.regexp "=\\?[_A-Za-z0-9-]+\\?\\([BbQq]\\)\\?\\([^?]*\\)\\?="

let decode_header s =
  let decode_group s =
    let enc = Str.matched_group 1 s
    and txt = Str.matched_group 2 s in
    match enc with
      "B" | "b" -> decode_base64 txt
    | "Q" | "q" -> decode_qp txt
    | _ -> assert false in
  Str.global_substitute re_encoded_header decode_group s

let re_continuation = Str.regexp "\n[ \t]+"
let re_nl = Str.regexp "\n"
let re_field = Str.regexp "\\([A-Za-z-]+[: ]\\)[ \t]*\\(.*\\)"

let parse_header s =
  let rec parse_field accu = function
    [] -> List.rev accu
  | line :: rem ->
      if Str.string_match re_field line 0 then begin
        let field_name = String.lowercase_ascii (Str.matched_group 1 line)
        and field_val  = Str.matched_group 2 line in
        parse_field ((field_name, decode_header field_val) :: accu) rem
      end else
        parse_field accu rem in
  parse_field [] (Str.split re_nl (Str.global_replace re_continuation " " s))

let find_header name headers =
  try List.assoc name headers with Not_found -> ""

let re_nl_nl = Str.regexp "\n\n"
let re_multipart =
  Str.regexp_case_fold
    "multipart/.*boundary *= *\\(\"\\([^\"]+\\)\"\\|\\([^ \t]+\\)\\)"

let rec parse_message s =
  try
    let pos_sep = Str.search_forward re_nl_nl s 0 in
    let headers = parse_header (String.sub s 0 pos_sep) in
    let body = String.sub s (pos_sep + 2) (String.length s - pos_sep - 2) in
    let encoding = find_header "content-transfer-encoding:" headers in
    let ctype = find_header "content-type:" headers in
    if Str.string_match re_multipart ctype 0 then begin
      let boundary =
        try
          Str.matched_group 2 ctype
        with Not_found -> try
          Str.matched_group 3 ctype
        with Not_found ->
          assert false in
      let re_bound =
        Str.regexp ("--" ^ Str.quote boundary ^ "[ \t\n]*") in
      match Str.split_delim re_bound body with
        [] ->
          { headers = headers;
            body = decode encoding body;
            parts = [] }
      | blurb :: parts ->
          { headers = headers;
            body = decode encoding blurb;
            parts = List.map parse_message parts }
    end else
      { headers = headers;
        body = decode encoding body;
        parts = [] }
  with Not_found ->
    { headers = [];
      body = s;
      parts = [] }

let safe_remove fname = try Sys.remove fname with Sys_error _ -> ()

let run_body_through_external_converter cmd arg body =
  let infile = Filename.temp_file "spamoracle" ".data" in
  let outfile = Filename.temp_file "spamoracle" ".txt" in
  let oc = open_out_bin infile in
  let ic = open_in_bin outfile in
  output_string oc body;
  close_out oc;
  let retcode =
    Sys.command
      (Printf.sprintf "%s %s < %s > %s" 
                      cmd (Filename.quote arg) infile outfile) in
  if retcode <> 0 then begin
    close_in ic;
    safe_remove infile; safe_remove outfile;
    None
  end else begin
    let len = in_channel_length ic in
    let res = really_input_string ic len in
    close_in ic;
    safe_remove infile; safe_remove outfile;
    Some res
  end

let header s msg =
  let rec hdr = function
    [] -> []
  | (h,v) :: rem -> if h = s then v :: hdr rem else hdr rem in
  String.concat "\n" (hdr msg.headers)

let header_matches s re msg =
  let rec hmatch = function
    [] -> false
  | (h,v) :: rem -> (h = s && Str.string_match re v 0) || hmatch rem
  in hmatch msg.headers

let re_content_text =
  Str.regexp_case_fold "text/plain\\|text/enriched\\|text;\\|text$"
let re_content_html =
  Str.regexp_case_fold "text/html"
let re_content_message_rfc822 =
  Str.regexp_case_fold "message/rfc822"
let re_content_alternative =
  Str.regexp_case_fold "multipart/alternative"
let re_content_multipart =
  Str.regexp_case_fold "multipart/"
let re_content_any_text =
  Str.regexp_case_fold "text/"

let rec iter_text_parts fn m =
  if header_matches "content-type:" re_content_text m 
  || not(List.mem_assoc "content-type:" m.headers) then
    fn m
  else if header_matches "content-type:" re_content_html m then
    fn {m with body = Htmlscan.extract_text m.body}
  else if header_matches "content-type:" re_content_alternative m then begin
    try
      if not !Config.alternative_favor_html then raise Not_found;
      iter_text_parts fn 
        (List.find (header_matches "content-type:" re_content_html) m.parts)
    with Not_found ->
      fn m;
      List.iter (iter_text_parts fn) m.parts
  end else if header_matches "content-type:" re_content_multipart m then begin
    fn m;
    List.iter (iter_text_parts fn) m.parts
  end else if header_matches "content-type:" re_content_message_rfc822 m then
    iter_text_parts fn (parse_message m.body)
  else if !Config.external_converter <> ""
       && not (header_matches "content-type:" re_content_any_text m)
       then begin
    match run_body_through_external_converter
              !Config.external_converter
              (header "content-type:" m)
              m.body with
    | None -> ()
    | Some txt -> fn {m with body = txt}
  end else
    ()

let iter_message fn msg =
  List.iter
    (fun (h, v) -> if Str.string_match !Config.mail_headers h 0 then fn v)
    msg.headers;
  iter_text_parts
    (fun m -> fn m.body)
    msg
