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

(* Processing messages *)

open Printf
open Mail
open Database
open Rankmsg

(* Mark message with rank info *)

let re_nl_nl = Str.regexp "\n\n"

let mark_message db txt =
  let m = parse_message txt in
  let r = rank_message db m in
  try
    let pos_sep = Str.search_forward re_nl_nl txt 0 in
    output stdout txt 0 pos_sep;
    let verdict =
      if r.spam_prob <= 0.2 && r.num_meaningful >= 5 then "no"
      else if r.spam_prob >= 0.8 && r.num_meaningful >= 5 then "yes"
      else "unknown" in
    printf "\nX-Spam: %s; %.2f; %s" verdict r.spam_prob r.explanation;
    let att = Attachments.summarize m in
    if att <> "" then
      printf "\nX-Attachments: %s" att;
    output stdout txt pos_sep (String.length txt - pos_sep)
  with Not_found ->
    print_string txt

(* Add messages to database *)

let record_words db is_spam txt =
  Wordsplit.iter
    (fun w ->
      if is_spam then add_spam db w else add_good db w)
    txt

let add_message db verbose is_spam msg =
  if verbose then begin
    printf "\r%6d / %6d" db.f_num_good db.f_num_spam;
    flush stdout
  end;
  iter_text_parts
    (fun m ->
       record_words db is_spam (header "from:" m);
       record_words db is_spam (header "subject:" m);
       record_words db is_spam m.body)
    (parse_message msg);
  if is_spam
  then db.f_num_spam <- db.f_num_spam + 1
  else db.f_num_good <- db.f_num_good + 1

(* Test analysis on a message *)

let test_message db low high txt =
  let msg = parse_message txt in
  let r = rank_message db msg in
  if r.spam_prob >= low && r.spam_prob <= high then begin
    printf "--------------------------------------------------\n";
    printf "From: %s\n" (header "from:" msg);
    printf "Subject: %s\n" (header "subject:" msg);
    printf "Score: %.2f -- %d\n" r.spam_prob r.num_meaningful;
    printf "Details: %s\n" r.explanation;
    let att = Attachments.summarize msg in
    if att <> "" then printf "Attachments: %s\n" att
  end

(* Statistics *)

type message_class = Msg_good | Msg_unknown | Msg_spam

let stat_message db txt =
  let msg = parse_message txt in
  let r = rank_message db msg in
  if r.spam_prob <= 0.2 && r.num_meaningful >= 5 then Msg_good
  else if r.spam_prob >= 0.8 && r.num_meaningful >= 5 then Msg_spam
  else Msg_unknown
