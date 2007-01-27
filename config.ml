(* Configurable parameters *)

let database_name =
  ref (try Filename.concat (Sys.getenv "HOME") ".spamoracle.db"
       with Not_found -> ".spamoracle.db")

let html_add_tags = ref false

let html_tag_attr = ref (Str.regexp_case_fold
  "a/href\\|img/src\\|img/alt\\|frame/src\\|font/face\\|font/color")

let mail_headers = ref (Str.regexp_case_fold
  "from:\\|subject:")

let alternative_favor_html = ref true

let spam_header = ref "X-Spam"

let attachments_header = ref "X-Attachments"

let summarize_attachments = ref true

let num_words_retained = ref 15

let max_repetitions = ref 2

let robinson_s = ref 0.0
let robinson_x = ref 0.5

let low_freq_limit = ref 0.01
let high_freq_limit = ref 0.99

let use_chi_square = ref false

let good_mail_prob = ref 0.2
let spam_mail_prob = ref 0.8

let min_meaningful_words = ref 5

let summarize_referenced = ref false

let referenced_header = ref "X-Referenced-Hosts"

let reassemble_words = ref false

let external_converter = ref ""

open Configfile

let options = [
  "database_file", String database_name;
  "html_retain_tags", Bool html_add_tags;
  "html_tag_attributes", Regexp html_tag_attr;
  "mail_headers", Regexp mail_headers;
  "alternative_favor_html", Bool alternative_favor_html;
  "spam_header", String spam_header;
  "attachments_header", String attachments_header;
  "summarize_attachments", Bool summarize_attachments;
  "referenced_header", String referenced_header;
  "summarize_referenced", Bool summarize_referenced;
  "num_meaningful_words", Int num_words_retained;
  "max_repetitions", Int max_repetitions;
  "low_freq_limit", Float low_freq_limit;
  "high_freq_limit", Float high_freq_limit;
  "min_meaningful_words", Int min_meaningful_words;
  "good_mail_prob", Float good_mail_prob;
  "spam_mail_prob", Float spam_mail_prob;
  "robinson_s", Float robinson_s;
  "robinson_x", Float robinson_x;
  "use_chi_square", Bool use_chi_square;
  "reassemble_words", Bool reassemble_words;
  "external_converter", String external_converter
]

