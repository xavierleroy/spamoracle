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

(* Word frequency database *)

type short = {
  s_num_good: int;
  s_num_spam: int;
  s_freq: (string, int * int) Hashtbl.t
}

type full = {
  mutable f_num_good: int;
  mutable f_num_spam: int;
  f_high_freq: (string, int * int) Hashtbl.t;
  f_low_freq: (string, int * int) Hashtbl.t
}

let magic = "Mailscrubber0001"

let check_magic filename ic =
  let buf = String.create (String.length magic) in
  really_input ic buf 0 (String.length magic);
  if buf <> magic then raise(Sys_error(filename ^ ": bad magic number"))

let read_short filename =
  let ic = open_in_bin filename in
  check_magic filename ic;
  let ng = input_binary_int ic in
  let ns = input_binary_int ic in
  let freq = Marshal.from_channel ic in
  close_in ic;
  { s_num_good = ng; s_num_spam = ns; s_freq = freq }

let read_full filename =
  let ic = open_in_bin filename in
  check_magic filename ic;
  let ng = input_binary_int ic in
  let ns = input_binary_int ic in
  let high_freq = Marshal.from_channel ic in
  let low_freq = Marshal.from_channel ic in
  close_in ic;
  { f_num_good = ng; f_num_spam = ns; 
    f_low_freq = low_freq; f_high_freq = high_freq }

let write_full filename db =
  let tempname = filename ^ ".tmp" in
  let oc =
    open_out_gen [Open_wronly; Open_trunc; Open_creat; Open_binary] 0o600
                 tempname in
  output_string oc magic;
  output_binary_int oc db.f_num_good;
  output_binary_int oc db.f_num_spam;
  Marshal.to_channel oc db.f_high_freq [Marshal.No_sharing];
  Marshal.to_channel oc db.f_low_freq [Marshal.No_sharing];
  close_out oc;
  Sys.rename tempname filename

let create sz =
  { f_num_good = 0;
    f_num_spam = 0;
    f_high_freq = Hashtbl.create sz;
    f_low_freq = Hashtbl.create sz }

let add_good db w =
  begin try
    let (g, s as f) = Hashtbl.find db.f_high_freq w in
    Hashtbl.replace db.f_high_freq w (g+1, s)
  with Not_found ->
  try
    let (g, s as f) = Hashtbl.find db.f_low_freq w in
    let g' = g + 1 in
    if 2 * g' + s >= 5 then begin
      Hashtbl.remove db.f_low_freq w;
      Hashtbl.add db.f_high_freq w (g', s)
    end else
      Hashtbl.replace db.f_low_freq w (g', s)
  with Not_found ->
    Hashtbl.add db.f_low_freq w (1, 0)
  end

let add_spam db w =
  begin try
    let (g, s) = Hashtbl.find db.f_high_freq w in
    Hashtbl.replace db.f_high_freq w (g, s+1)
  with Not_found ->
  try
    let (g, s) = Hashtbl.find db.f_low_freq w in
    let s' = s + 1 in
    if 2 * g + s' >= 5 then begin
      Hashtbl.remove db.f_low_freq w;
      Hashtbl.add db.f_high_freq w (g, s')
    end else
      Hashtbl.replace db.f_low_freq w (g, s')
  with Not_found ->
    Hashtbl.add db.f_low_freq w (0, 1)
  end

open Printf

let dump db oc =
  let dump_entry w (g, s) = fprintf oc "%s %d %d\n" w g s in
  fprintf oc "SPAMORACLE/1 %d %d\n" db.f_num_good db.f_num_spam;
  Hashtbl.iter dump_entry db.f_high_freq;
  Hashtbl.iter dump_entry db.f_low_freq

let split s =
  try
    let i = String.index s ' ' in
    let j = String.index_from s (i + 1) ' ' in
    (String.sub s 0 i,
     int_of_string (String.sub s (i + 1) (j - i - 1)),
     int_of_string (String.sub s (j + 1) (String.length s - j - 1)))
  with Not_found ->
    failwith("Database restoration: ill-formed line `"
             ^ String.escaped s ^ "'")

let restore ic =
  let db = create 997 in
  begin try
    let (w, ng, ns) = split (input_line ic) in
    if w <> "SPAMORACLE/1"
    then failwith("Database restoration: wrong version");
    db.f_num_good <- ng;  
    db.f_num_spam <- ns
  with End_of_file ->
    failwith("Database restoration: first line missing");
  end;
  begin try
    while true do
      let (w, g, s) = split (input_line ic) in
      if 2 * g + s >= 5
      then Hashtbl.add db.f_high_freq w (g, s)
      else Hashtbl.add db.f_low_freq w (g, s)
    done
  with End_of_file ->
    ()
  end;
  db
