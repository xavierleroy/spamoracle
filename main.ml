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

let default_config_name =
  try Filename.concat (Sys.getenv "HOME") ".spamoracle.conf"
  with Not_found -> ".spamoracle.conf"

let parse_config_file file =
  try
    let errs = Configfile.parse Config.options file in
    if errs <> [] then begin
      eprintf "Error while reading configuration file %s:\n" file;
      List.iter (fun (line, msg) -> eprintf "Line %d: %s\n" line msg) errs;
      exit 2
    end
  with Sys_error msg ->
    eprintf "Cannot reading configuration file %s:\n%s\n" file msg

let mark_command args =
  let db = Database.read_short !Config.database_name in
  if args = [] then
    mark_message db (read_single_msg stdin)
  else
    List.iter (fun f -> mbox_file_iter f (mark_message db)) args

let add_command args =
  let db =
    try
      Database.read_full !Config.database_name
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
  in parse_args args; Database.write_full !Config.database_name db

let list_command args =
  let db = Database.read_full !Config.database_name in
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
              max !Config.low_freq_limit
                  (min !Config.high_freq_limit
                       (pspam /. (pgood +. pspam)))
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
  let db = Database.read_short !Config.database_name in
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

let stat_command args =
  let db = Database.read_short !Config.database_name in
  let stat_mbox f =
    let num_msgs = ref 0
    and num_good = ref 0
    and num_spam = ref 0
    and num_unknown = ref 0 in
    mbox_file_iter f
      (fun s ->
        incr num_msgs;
        match stat_message db s with
          Msg_good -> incr num_good
        | Msg_spam -> incr num_spam
        | Msg_unknown -> incr num_unknown);
    let percentage a b =
      100.0 *. float a /. float b in
    if !num_msgs > 0 then
      printf "%s: %.2f%% good, %.2f%% unknown, %.2f%% spam\n"
             f 
             (percentage !num_good !num_msgs)
             (percentage !num_unknown !num_msgs)
             (percentage !num_spam !num_msgs)
  in List.iter stat_mbox args

let backup_command () =
  Database.dump (Database.read_full !Config.database_name) stdout

let restore_command () =
  Database.write_full !Config.database_name (Database.restore stdin)

let rec parse_args_1 = function
    "-config" :: file :: rem ->
      parse_config_file file; parse_args_2 rem
  | "-config" :: [] ->
      raise(Usage("Option -config requires an argument"))
  | rem ->
      if Sys.file_exists default_config_name
      then parse_config_file default_config_name;
      parse_args_2 rem

and parse_args_2 = function
  | "-f" :: file :: rem ->
      Config.database_name := file; parse_args_3 rem
  | "-f" :: [] ->
      raise(Usage("Option -f requires an argument"))
  | rem ->
      parse_args_3 rem

and parse_args_3 = function
    "mark" :: rem ->
      mark_command rem
  | "add" :: rem ->
      add_command rem
  | "list" :: rem ->
      list_command rem
  | "test" :: rem ->
      test_command rem
  | "stat" :: rem ->
      stat_command rem
  | "backup" :: rem ->
      backup_command ()
  | "restore" :: rem ->
      restore_command ()
  | s :: rem ->
      raise(Usage("Unknown command " ^ s))
  | [] ->
      raise(Usage "")

let usage_string = "\
Usage:
  spamoracle [-config conf] [-f db] mark {mailbox}*
  Add 'X-Spam:' headers to messages with result of analysis
    {mailbox}*   Mailboxes containing messages to analyze and mark
                 If none given, read single msg from standard input

  spamoracle [-config conf] [-f db] add [-v] -spam {spambox}* -good {goodbox}*
  Create or update database with known spam or non-spam messages
    -v           Print progress bar
    -spam        Indicate subsequent mailboxes contain spam
    -good        Indicate subsequent mailboxes contain good msgs (not spam)
    {spambox}*   Mailboxes containing spam
    {goodbox}*   Mailboxes containing good messages (not spam)
                 If no mailbox given, read single msg from standard input

  spamoracle [-config conf] [-f db] test [-min prob] [-max prob] {mailbox}*
  Analyze messages and print summary of results for each message
    -min <prob>  Don't print messages with result below <prob>   
    -max <prob>  Don't print messages with result above <prob>   
    {mailbox}*   Mailboxes containing messages to analyze

  spamoracle [-config conf] [-f db] stat {mailbox}*
  Analyze messages and print percentages of spam/non-spam for each mailbox
    {mailbox}*   Mailboxes containing messages to analyze

  spamoracle [-config conf] [-f db] list {regexp}*
  Dump word statistics in database
    {regexp}*    Regular expressions for words we are interested in

  spamoracle [-config conf] [-f db] backup > database.backup
  Dump whole database in portable text format on standard output

  spamoracle [-config conf] [-f db] restore < database.backup
  Restore database from text backup file read from standard input

  Common options:
    -config <conf> Configuration file (default $HOME/.spamoracle.conf)
    -f <db>        Database to use (default $HOME/.spamoracle.db)"

let main () =
  try
    parse_args_1 (List.tl (Array.to_list Sys.argv))
  with
  | Usage msg ->
      eprintf "%s\n%s\n" msg usage_string;
      exit 2
  | Sys_error msg ->
      eprintf "System error: %s\n" msg
  | Database.Error msg ->
      eprintf "%s\n" msg

let _ = main()

