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

(* Reading of a mailbox file and splitting into individual messages *)

type t =
  { ic: in_channel;
    zipped: bool;
    mutable start: string;
    buf: Buffer.t }

let open_mbox_file filename =
  if Filename.check_suffix filename ".gz" then
    { ic = Unix.open_process_in ("gunzip -c " ^filename);
      zipped = true;
      start = "";
      buf = Buffer.create 50000 }
  else
    { ic = open_in filename;
      zipped = false;
      start = "";
      buf = Buffer.create 50000 }

let open_mbox_channel ic =
    { ic = ic;
      zipped = false;
      start = "";
      buf = Buffer.create 50000 }

let re_crlf = Str.regexp "\r+\n"

let normalize_eol s =
  match String.index s '\r' with
  | exception Not_found -> s
  | _ -> Str.global_replace re_crlf "\n" s

let read_msg t =
  Buffer.clear t.buf;
  Buffer.add_string t.buf t.start;
  let rec read () =
    let line = input_line t.ic in
    if String.length line >= 5
    && String.sub line 0 5 = "From "
    && Buffer.length t.buf > 0 then begin
      t.start <- (line ^ "\n");
      normalize_eol (Buffer.contents t.buf)
    end else begin
      Buffer.add_string t.buf line;
      Buffer.add_char t.buf '\n';
      read ()
    end in
  try
    read()
  with End_of_file ->
    if Buffer.length t.buf > 0 then begin
      t.start <- "";
      normalize_eol (Buffer.contents t.buf)
    end else
      raise End_of_file

let close_mbox t =
  if t.zipped
  then ignore(Unix.close_process_in t.ic)
  else close_in t.ic

let mbox_file_iter filename fn =
  let ic = open_mbox_file filename in
  try
    while true do fn(read_msg ic) done
  with End_of_file ->
    close_mbox ic

let mbox_channel_iter inchan fn =
  let ic = open_mbox_channel inchan in
  try
    while true do fn(read_msg ic) done
  with End_of_file ->
    close_mbox ic

let read_single_msg inchan =
  let res = Buffer.create 10000 in
  let buf = Bytes.create 1024 in
  let rec read () =
    let n = input inchan buf 0 (Bytes.length buf) in
    if n > 0 then begin
      Buffer.add_subbytes res buf 0 n;
      read ()
    end in
  read ();
  normalize_eol (Buffer.contents res)

let maildir_iter dirname fn =
  let dir = Filename.concat dirname "cur" in
  let dh = Unix.opendir dir in
  try
    while true do
      let f = Unix.readdir dh in
      if f <> "." && f <> ".." then begin
        try
          let ic = open_mbox_file (Filename.concat dir f) in
          fn (read_msg ic); (* maildir contains one mail per file *)
          close_mbox ic;
        with Sys_error _ -> ()
        (* maildir file might concurrently disappear *)
      end
    done
  with End_of_file ->
   Unix.closedir dh

let mbox_iter name fn =
  let dir = Filename.concat name "cur" in
  if Sys.file_exists dir && Sys.is_directory dir then
    maildir_iter name fn
  else
    mbox_file_iter name fn
