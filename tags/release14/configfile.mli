(* Parsing configuration files *)

type value =
  | Bool of bool ref
  | String of string ref
  | Int of int ref
  | Float of float ref
  | Regexp of Str.regexp ref
  | OptBool of bool option ref
  | OptString of string option ref
  | OptInt of int option ref
  | OptFloat of float option ref
  | OptRegexp of Str.regexp option ref

val parse: (string * value) list -> string -> (int * string) list
