(** Message ranking *)

type rank =
  { spam_prob: float;
    num_meaningful: int;
    explanation: string }

val rank_message: Database.short -> Mail.message -> rank

