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

(** Word frequency database *)

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

val read_short: string -> short
val read_full: string -> full
val write_full: string -> full -> unit
val create: int -> full
val add_good: full -> string -> unit
val add_spam: full -> string -> unit
val dump: full -> out_channel -> unit
val restore: in_channel -> full
val in_short: short -> string -> bool
val in_full: full -> string -> bool

exception Error of string
