(** Decompose a string into words. *)

val iter: (string -> unit) -> string -> unit
  (** [iter fn txt] applies [fn] to each word in [txt].
      A word is a sequence of 3 to 12 word characters.
      Word characters include letters, numbers, French accented letters,
      dashes, single quotes, dollar and percent signs.
      Words are converted to lowercase and accents on letters are
      removed before they are passed to [fn].
      In addition, for each run of 3 or more uppercase letters,
      [fn] is applied to ["Ux"] where [x] is the length of the run,
      e.g. ["U8"] for 8 uppercase letters in a row.
      Similarly, for each run of 3 or more characters above 127,
      [fn] is applied to ["Wx"] where [x] is the length of the run. *)
