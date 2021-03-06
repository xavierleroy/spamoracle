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

(** Parsing of e-mail messages, including attachments *)

type message =
  { headers: (string * string) list;
    body: string;
    parts: message list }
  (** The type of parsed e-mail messages.
    - [headers] is an A-list of pairs [(header-name, header-content)].
      [header-name] is lowercased and includes [:], e.g. [subject:].
    - [body] is the body of the message.  Base64 and quoted-printable
      encodings are already decoded.  For multipart messages, [body]
      is the initial blurb before the first part.
    - [parts] is empty except for multipart messages, in which case
      it lists all parts, recursively represented as messages. *)

val parse_message: string -> message
  (** Parse the given textual message and return its structure. *)

val header: string -> message -> string
  (** [header h msg] returns the contents of header named [h]
      in message [msg], or the empty string if this header is missing.
      Remember that header names are lowercased and include the final [:],
      e.g. [subject:]. *)

val iter_text_parts: (message -> unit) -> message -> unit
  (** [iter_text_parts fn msg] applies [fn] to every (sub-)message 
      contained in [msg] that is of type text. *)

val iter_message: (string -> unit) -> message -> unit
  (** [iter_message fn msg] applies [fn] to the following parts of
      message [msg]:
      - the headers that match [!Config.mail_headers];
      - the body of every sub-message of [msg] that is of type text.
    *)
