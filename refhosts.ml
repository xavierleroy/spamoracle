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

(* Extract hostnames or IP addresses referenced from http URLs
   in message bodies. *)

let re_url =
  Str.regexp_case_fold
    "http://\\([^@]+@\\)?\\([a-z0-9-]+\\(\\.[a-z0-9-]+\\)+\\)"

module StringSet = Set.Make(String)

let hosts = ref StringSet.empty

let reset() = hosts := StringSet.empty

let rec add_urls txt pos =
  let matched =
    try ignore (Str.search_forward re_url txt pos); true
    with Not_found -> false in
  if matched then begin
    hosts := StringSet.add (Str.matched_group 2 txt) !hosts;
    add_urls txt (Str.match_end())
  end

let add txt =
  add_urls txt 0

let summarize () =
  let lst = StringSet.elements !hosts in
  hosts := StringSet.empty;
  String.concat " " lst
