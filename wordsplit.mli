(** Decompose a string into words. *)

val iter: (string -> unit) -> (string -> bool) -> string -> unit
  (** [iter fn db txt] applies [fn] to each word in [txt].
      [db] is a predicate that returns [true] for known words.
      It is used to recognize stretched-out words, e.g. [H.e.ll.o].
 *)
