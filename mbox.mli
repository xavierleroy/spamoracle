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

(** Reading of a mailbox file and splitting into individual messages *)

type t
  (** The type of channels opened on a mailbox *)

val open_mbox_file: string -> t
  (** Open the given file name as a mailbox, and return an mbox channel
      ready for reading.  If the file name ends in [.gz], arrange
      for on-the-fly decompression with [zcat]. *)
val open_mbox_channel: in_channel -> t
  (** Open the given input channel as a mailbox. *)
val read_msg: t -> string
  (** Read the next message from the given channel, and return it
      as a string.  Raise [End_of_file] if no message remains. *)
val close_mbox: t -> unit
  (** Close the given mbox channel. *)
val mbox_file_iter: string -> (string -> unit) -> unit
  (** [mbox_file_iter filename fn] reads messages from the file named
      [filename], and applies [fn] in turn to each message. *)
val mbox_channel_iter: in_channel -> (string -> unit) -> unit
  (** [mbox_channel_iter ic fn] reads messages from the input channel
      [ic], and applies [fn] in turn to each message. *)

val maildir_iter: string -> (string -> unit) -> unit
  (** [maildir_iter dirname fn] reads messages from the maildir
      [<dirname>/cur], and applies [fn] in turn to each message. *)

val mbox_iter: string -> (string -> unit) -> unit
  (** [mbox_iter name fn] runs {!maildir_iter} if [name] is a Maildir
      format directory (i.e. [<name>/cur] is a directory), and
      otherwise opens [name] as an mbox-format file. *)

val read_single_msg: in_channel -> string
  (** Read one message from the given channel, up to end of file *)
