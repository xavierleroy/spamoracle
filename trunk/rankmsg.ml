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

(* Message ranking *)

open Mail
open Database

let word_count_in w res =
  let count = ref 0 in
  for i = 0 to Array.length res - 1 do
    if w = fst res.(i) then incr count
  done;
  !count

let add_word w p res =
  let i = ref 0 in
  while !i < Array.length res
     && abs_float (p -. 0.5) <= abs_float(snd res.(!i) -. 0.5)
  do
    incr i
  done;
  if !i < Array.length res then begin
    for j = Array.length res - 1 downto !i + 1 do
      res.(j) <- res.(j - 1)
    done;
    res.(!i) <- (w, p)
  end

let normalize (p : float) low high =
  if p > high then high else if p < low then low else p

let cap (p : float) =
  if p > 1.0 then 1.0 else p

let word_proba g b num_g num_b =
  let g = 2 * g in  (* Graham's magic factor to bias in favor of ham *)
  let pgood = cap (float g /. float num_g)
  and pbad = cap (float b /. float num_b) in
  let p = pbad /. (pgood +. pbad) in
  (* Robinson's adjustement *)
  let n = float (g + b) in
  let p =
    (!Config.robinson_s *. !Config.robinson_x +. n *. p)
    /. (!Config.robinson_s +. n) in
  (* Result normalization *)
  normalize p !Config.low_freq_limit !Config.high_freq_limit

let process_word (db, res) w =
  try
    let (g, b) = Hashtbl.find db.s_freq w in
    if word_count_in w res < !Config.max_repetitions then begin
      let p = word_proba g b db.s_num_good db.s_num_spam in
      add_word w p res
    end
  with Not_found ->
    ()

let process_words ((db, res) as ctx) txt =
  Wordsplit.iter (process_word ctx) (in_short db) txt;
  if !Config.summarize_referenced then Refhosts.add txt

let process_msg ctx m =
  iter_message (process_words ctx) m

(* This is Graham's original approach *)
(*
let spaminess_score res =
  let probs = List.map snd (Array.to_list res) in
  let prod = List.fold_left ( *. ) 1.0 probs
  and cprod = List.fold_left ( *. ) 1.0 (List.map (fun x -> 1.0 -. x) probs) in
  prod /. (prod +. cprod)
*)

(* This is Robinson's chi-square stuff *)

let chi2_inverse m n =   (* chi2 inverse of 2m with 2n degrees *)
  let t = ref (exp (-. m)) in
  let s = ref !t in
  for i = 1 to n do
    t := !t *. m /. float i;
    s := !s +. !t
  done;
  if !s >= 1.0 then 1.0 else !s

let log2 = log 2.0

let chi2_hypothesis ps =
  (* Compute -2 * ln (product ps).  Be careful with underflows. *)
  let p = ref 1.0 and pexp = ref 0 in
  for i = 0 to Array.length ps - 1 do
    p := !p *. ps.(i);
    if !p <= 1e-200 then begin
      let (x, e) = frexp !p in p := x; pexp := !pexp + e
    end
  done;
  chi2_inverse (-. (log !p +. log2 *. float !pexp)) (Array.length ps)

let spaminess_score res =
  let probs = Array.map snd res in
  let cprobs = Array.map (fun x -> 1.0 -. x) probs in
  0.5 *. (1.0 +. chi2_hypothesis probs -. chi2_hypothesis cprobs)

type rank =
  { spam_prob: float;
    num_meaningful: int;
    explanation: string }

let rank_message db msg =
  Refhosts.reset();
  let res = Array.make !Config.num_words_retained ("", 0.5) in
  process_msg (db, res) msg;
  let p = spaminess_score res in
  let meaningful = ref 0 in
  while !meaningful < Array.length res && fst res.(!meaningful) <> ""
  do incr meaningful done;
  let summary = Buffer.create 200 in
  for i = 0 to !meaningful - 1 do
    let (w, p) = res.(i) in
    Printf.bprintf summary "%s:%02d " w (truncate (p *. 100.0))
  done;
  { spam_prob = p;
    num_meaningful = !meaningful;
    explanation = Buffer.contents summary }

