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

val mark_message : Database.short -> string -> unit
val record_words : Database.full -> bool -> string -> unit
val add_message : Database.full -> bool -> bool -> string -> unit
val test_message : Database.short -> float -> float -> string -> string -> unit
type message_class = Msg_good | Msg_unknown | Msg_spam
val stat_message : Database.short -> string -> message_class
val wordsplit_message : string -> unit
