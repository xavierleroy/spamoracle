(* Reading of a mailbox file and splitting into individual messages *)

type t =
  { ic: in_channel;
    zipped: bool;
    mutable start: string;
    buf: Buffer.t }

let open_mbox_file filename =
  if Filename.check_suffix filename ".gz" then
    { ic = Unix.open_process_in ("zcat " ^filename);
      zipped = true;
      start = "";
      buf = Buffer.create 50000 }
  else
    { ic = open_in filename;
      zipped = false;
      start = "";
      buf = Buffer.create 50000 }

let open_mbox_channel ic =
    { ic = ic;
      zipped = false;
      start = "";
      buf = Buffer.create 50000 }

let read_msg t =
  Buffer.clear t.buf;
  Buffer.add_string t.buf t.start;
  let rec read () =
    let line = input_line t.ic in
    if String.length line >= 5
    && String.sub line 0 5 = "From "
    && Buffer.length t.buf > 0 then begin
      t.start <- (line ^ "\n");
      Buffer.contents t.buf
    end else begin
      Buffer.add_string t.buf line;
      Buffer.add_char t.buf '\n';
      read ()
    end in
  try
    read()
  with End_of_file ->
    if Buffer.length t.buf > 0 then begin
      t.start <- "";
      Buffer.contents t.buf
    end else
      raise End_of_file

let close_mbox t =
  if t.zipped
  then ignore(Unix.close_process_in t.ic)
  else close_in t.ic

let mbox_file_iter filename fn =
  let ic = open_mbox_file filename in
  try
    while true do fn(read_msg ic) done
  with End_of_file ->
    close_mbox ic

let mbox_channel_iter inchan fn =
  let ic = open_mbox_channel inchan in
  try
    while true do fn(read_msg ic) done
  with End_of_file ->
    close_mbox ic

