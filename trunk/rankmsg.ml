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

let process_word (db, res) w =
  try
    let (g, b) = Hashtbl.find db.s_freq w in
    if word_count_in w res < 2 then begin
      let g = 2 * g in
      let pgood = float g /. float db.s_num_good
      and pbad = float b /. float db.s_num_spam in
      let p = max 0.01 (min 0.99 (pbad /. (pgood +. pbad))) in
      add_word w p res
    end
  with Not_found ->
    ()

let process_words ctx txt =
  Wordsplit.iter (process_word ctx) txt

let process_msg ctx m =
  iter_text_parts
    (fun m ->
      process_words ctx (header "from:" m);
      process_words ctx (header "subject:" m);
      process_words ctx m.body)
    m

let bayes_rule res =
  let probs = List.map snd (Array.to_list res) in
  let prod = List.fold_left ( *. ) 1.0 probs
  and cprod = List.fold_left ( *. ) 1.0 (List.map (fun x -> 1.0 -. x) probs) in
  prod /. (prod +. cprod)

type rank =
  { spam_prob: float;
    num_meaningful: int;
    explanation: string }

let rank_message db msg =
  let res = Array.make 15 ("", 0.5) in
  process_msg (db, res) msg;
  let p = bayes_rule res in
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

