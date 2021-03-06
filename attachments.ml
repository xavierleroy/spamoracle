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

(* Summarize the attachments of a message as one line that can be
   put in the header of the message.  Allows procmail to filter
   suspicious attachments without looking at the message body. *)

open Printf
open Mail

let re_content_type =
  Str.regexp "\\([/a-zA-Z0-9-]+\\)"
let re_innocuous_content_types =
  Str.regexp_case_fold "text/plain\\|text/html\\|text/x-vcard\\|multipart/\\|message/rfc822\\|message/delivery-status"
let re_charset =
  Str.regexp_case_fold "charset=\\(\"\\([^\"]+\\)\"\\|[^ \t;]+\\)"
let re_innocuous_charsets =
  Str.regexp_case_fold "us-ascii\\|iso[-_]8859[-_]1$\\|iso[-_]8859[-_]15\\|windows-1252"
let re_name =
  Str.regexp_case_fold "name=\\(\"\\([^\"]+\\)\"\\|[^ \t;]+\\)"

let match_anchored re s =
  Str.string_match re s 0
let match_unanchored re s =
  try ignore (Str.search_forward re s 0); true with Not_found -> false

let summarize msg =
  let res = Buffer.create 200 in
  let rec summ m =
    let h = header "content-type:" m in
    if match_anchored re_content_type h then begin
      let c = Str.matched_group 1 h in
      if not (Str.string_match re_innocuous_content_types c 0) then
        bprintf res "type=\"%s\" " c
    end;
    if match_unanchored re_charset h then begin
      let c =
        try Str.matched_group 2 h with Not_found -> Str.matched_group 1 h in
      if not (Str.string_match re_innocuous_charsets c 0) then
        bprintf res "cset=\"%s\" " c
    end;
    if match_unanchored re_name h then begin
      let c =
        try Str.matched_group 2 h with Not_found -> Str.matched_group 1 h in
      bprintf res "name=\"%s\" " c
    end;
    let h = header "content-disposition:" m in
    if match_unanchored re_name h then begin
      let c =
        try Str.matched_group 2 h with Not_found -> Str.matched_group 1 h in
      bprintf res "name=\"%s\" " c
    end;
    List.iter summ m.parts in
  List.iter summ msg.parts;
  Buffer.contents res
