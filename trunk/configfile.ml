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

exception Error of string

let re_bool_yes = Str.regexp_case_fold "\\(on\\|yes\\|true\\|1\\)[ \t\r]*$"
let re_bool_no  = Str.regexp_case_fold "\\(off\\|no\\|false\\|0\\)[ \t\r]*$"

let parse_bool data =
  if Str.string_match re_bool_yes data 0 then true
  else if Str.string_match re_bool_no data 0 then false
  else raise (Error "invalid boolean value")

let trim_spaces s =
  let i = ref (String.length s - 1) in
  while !i >= 0 && (let c = s.[!i] in c = ' ' || c = '\t' || c = '\r')
  do decr i done;
  String.sub s 0 (!i + 1)

let parse_string data =
  try
    Scanf.sscanf data "%S" (fun s -> s)
  with Scanf.Scan_failure _ ->
    trim_spaces data

let parse_int data =
  try
    Scanf.sscanf data "%i" (fun n -> n)
  with Scanf.Scan_failure _ | Failure _ ->
    raise (Error ("invalid integer value"))

let parse_float data =
  try
    Scanf.sscanf data "%f" (fun n -> n)
  with Scanf.Scan_failure _ | Failure _ ->
    raise (Error ("invalid floating-point value"))

let parse_regexp data =
  try
    Str.regexp_case_fold (trim_spaces data)
  with Failure msg ->
    raise (Error ("invalid regular expression: " ^ msg))

let parse_data valuedesc data =
  match valuedesc with
  | Bool r      -> r := parse_bool data
  | String r    -> r := parse_string data
  | Int r       -> r := parse_int data
  | Float r     -> r := parse_float data
  | Regexp r    -> r := parse_regexp data
  | OptBool r   -> r := Some(parse_bool data)
  | OptString r -> r := Some(parse_string data)
  | OptInt r    -> r := Some(parse_int data)
  | OptFloat r  -> r := Some(parse_float data)
  | OptRegexp r -> r := Some(parse_regexp data)

let re_line =
  Str.regexp "[ \t]*\\([A-Za-z][A-Za-z0-9_]*\\)[ \t]*=[ \t]*\\(.*\\)"
let re_skip =
  Str.regexp "#\\|[ \t\r]*$"

let parse_line opts s =
  if Str.string_match re_line s 0 then begin
    let key = Str.matched_group 1 s and data = Str.matched_group 2 s in
    try
      parse_data (List.assoc key opts) data
    with Not_found ->
      raise (Error ("unknown variable " ^ key))
  end
  else if not (Str.string_match re_skip s 0) then
    raise (Error "ill-formed line")
    
let parse opts filename =
  let ic = open_in filename in
  let lineno = ref 1 in
  let errors = ref [] in
  begin try
    while true do
      let s = input_line ic in
      begin try
        parse_line opts s
      with Error msg ->
        errors := (!lineno, msg) :: !errors
      end;
      incr lineno
    done
  with End_of_file ->
    close_in ic
  end;
  List.rev !errors
