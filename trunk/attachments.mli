(* Summarize the attachments of a message as one line that can be
   put in the header of the message.  Allows procmail to filter
   suspicious attachments without looking at the message body. *)

val summarize: Mail.message -> string
