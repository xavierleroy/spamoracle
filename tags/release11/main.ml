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

(* Argument parsing and main program *)

open Printf
open Mbox
open Database
open Processing

exception Usage of string

let database_name =
  ref (try Filename.concat (Sys.getenv "HOME") ".spamoracle.db"
       with Not_found -> ".spamoracle.db")

let mark_command args =
  let db = Database.read_short !database_name in
  if args = [] then
    mark_message db (read_single_msg stdin)
  else
    List.iter (fun f -> mbox_file_iter f (mark_message db)) args

let add_command args =
  let db =
    try
      Database.read_full !database_name
    with Sys_error _ ->
      Database.create 997 in
  let processed = ref false
  and is_spam = ref true
  and verbose = ref false in
  let rec parse_args = function
    | "-v" :: rem ->
        verbose := true; parse_args rem
    | "-spam" :: rem ->
        is_spam := true; parse_args rem
    | "-good" :: rem ->
        is_spam := false; parse_args rem
    | f :: rem ->
        mbox_file_iter f (add_message db !verbose !is_spam);
        processed := true;
        parse_args rem
    | [] ->
        if not !processed then
          add_message db !verbose !is_spam (read_single_msg stdin);
        if !verbose then
          printf "\r%6d / %6d  good / spam messages\n"
                 db.f_num_good db.f_num_spam
  in parse_args args; Database.write_full !database_name db

let list_command args =
  let db = Database.read_full !database_name in
  let res = ref [] in
  List.iter
    (fun s ->
      let re = Str.regexp (s ^ "$") in
      let match_word w (g, s) =
        if Str.string_match re w 0 then begin
          let p =
            if 2 * g + s < 5 then -1.0 else begin
              let pgood = float (2 * g) /. float db.f_num_good
              and pspam = float s /. float db.f_num_spam in
              max 0.01 (min 0.99 (pspam /. (pgood +. pspam)))
            end in
          res := (w, p, g, s) :: !res
        end in
      Hashtbl.iter match_word db.f_high_freq;
      Hashtbl.iter match_word db.f_low_freq)
    args;
  if !res = [] then
    Printf.printf "No matching word found in database.\n"
  else begin
    Printf.printf "     Word         Proba   #good   #spam\n";
    List.iter
      (fun (w, p, g, s) ->
        if p >= 0.0 then
          Printf.printf "%-15s%8.2f%8d%8d\n" w p g s
        else
          Printf.printf "%-15s    ----%8d%8d\n" w g s)
      (List.sort
        (fun (_, p1, _, _) (_, p2, _, _) -> compare p2 p1)
        !res)
  end

let test_command args =
  let db = Database.read_short !database_name in
  let low = ref 0.0 and high = ref 1.0 in
  let rec parse_args = function
    | "-min" :: s :: rem ->
        begin try low := float_of_string s
        with Failure _ -> raise(Usage("bad argument to -min"))
        end;
        parse_args rem
    | "-min" :: [] ->
        raise(Usage("no argument to -min"))
    | "-max" :: s :: rem ->
        begin try high := float_of_string s
        with Failure _ -> raise(Usage("bad argument to -max"))
        end;
        parse_args rem
    | "-max" :: [] ->
        raise(Usage("no argument to -max"))
    | f :: rem ->
        mbox_file_iter f (test_message db !low !high);
        parse_args rem
    | [] -> ()
  in parse_args args

let rec parse_args_1 = function
  | "-f" :: file :: rem ->
      database_name := file; parse_args_2 rem
  | "-f" :: [] ->
      raise(Usage("Option -f requires an argument"))
  | rem ->
      parse_args_2 rem

and parse_args_2 = function
    "mark" :: rem ->
      mark_command rem
  | "add" :: rem ->
      add_command rem
  | "list" :: rem ->
      list_command rem
  | "test" :: rem ->
      test_command rem
  | s :: rem ->
      raise(Usage("Unknown command " ^ s))
  | [] ->
      raise(Usage "")

let main () =
  try
    parse_args_1 (List.tl (Array.to_list Sys.argv))
  with
  | Usage msg ->
      eprintf "%s\n" msg;
      eprintf "\
Usage:
  spamoracle [-f db] mark {mailbox}*
  Add 'X-Spam:' headers to messages with result of analysis
    -f <db>      Database to use (default $HOME/.spamoracle.db)
    {mailbox}*   Mailboxes containing messages to markup
                 If none given, read single msg from standard input

  spamoracle [-f db] add [-v] -spam {spambox}* -good {goodbox}*
  Create or update database with known spam or non-spam messages
    -f <db>      Database to use (default $HOME/.spamoracle.db)
    -v           Print progress bar
    -spam        Indicate subsequent mailboxes contain spam
    -good        Indicate subsequent mailboxes contain good msgs (not spam)
    {spambox}*   Mailboxes containing spam
    {goodbox}*   Mailboxes containing good messages (not spam)
                 If no mailbox given, read single msg from standard input

  spamoracle [-f db] test [-min prob] [-max prob] {mailbox}*
  Analyze messages and print summary of results
    -f <db>      Database to use (default $HOME/.spamoracle.db)
    -min <prob>  Don't print messages with result below <prob>   
    -max <prob>  Don't print messages with result above <prob>   
    {mailbox}*   Mailboxes containing messages to analyze

  spamoracle [-f db] list {regexp}*
  Dump word statistics in database
    -f <db>      Database to use (default $HOME/.spamoracle.db)
    {regexp}*    Regular expression for words we are interested in
";
      exit 2
  | Sys_error msg ->
      eprintf "System error: %s\n" msg

let _ = main()

