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
  (** Lower limit for word frequencies.  Default is 0.001. *)

val high_freq_limit : float ref
  (** Upper limit for word frequencies.  Default is 0.999. *)

val robinson_s : float ref
val robinson_x : float ref
  (** Robinson's parameters for taking word frequencies into account. *)

val use_chi_square : bool ref
  (** Use Robinson's chi-square test *)

val min_meaningful_words : int ref
  (** Number of meaningful words below which mails are classified as unknown *)
val good_mail_prob : float ref
  (** Spam probability below which mails are classified as good *)
val spam_mail_prob : float ref
  (** Spam probability below which mails are classified as spam *)

val summarize_referenced : bool ref

val referenced_header : string ref

val reassemble_words : bool ref

val external_converter : string ref
  (** Program to be called on message parts that are not text.
      The program receives the content-type as first argument
      and the actual data on standard input.
      It should output the corresponding text on standard output,
      or exit with non-zero error code if it cannot extract text. *)

val options : (string * Configfile.value) list
  (** List of configurable parameters *)
