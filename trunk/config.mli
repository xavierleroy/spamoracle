(* Configurable parameters *)

val database_name : string ref
  (** Name of database file *)

val html_add_tags : bool ref
  (** Whether to treat HTML tag names as words *)

val html_tag_attr : Str.regexp ref
  (** Regexp matching [tag/attr] strings denoting pairs of HTML tag and
      attribute names.  If a tag and attribute pair matches, the associated
      value is added to the text. *)

val mail_headers : Str.regexp ref
  (** Regexp matching names of e-mail headers that must be analyzed. *)

val alternative_favor_html : bool ref
  (** If true, consider only the HTML part of a multipart/alternative.
      Otherwise, consider all parts. *)

val spam_header : string ref
  (** Name of header added with spam / not-spam info (default: "X-Spam") *)

val attachments_header : string ref
  (** Name of header added with attachment summary (default: "X-Attachments") *)

val summarize_attachments : bool ref
  (** Whether to generate the attachment summary *)

val num_words_retained : int ref
  (** Number of meaningful words to retain for computing final prob. *)

val max_repetitions : int ref
  (** Among the meaningful words, max number of time a given word
      can appear. *)

val low_freq_limit : float ref
  (** Lower limit for word frequencies.  Default is 0.01. *)

val high_freq_limit : float ref
  (** Upper limit for word frequencies.  Default is 0.99. *)

val min_meaningful_words : int ref
  (** Number of meaningful words below which mails are classified as unknown *)
val good_mail_prob : float ref
  (** Spam probability below which mails are classified as good *)
val spam_mail_prob : float ref
  (** Spam probability below which mails are classified as spam *)

val options : (string * Configfile.value) list
  (** List of configurable parameters *)
