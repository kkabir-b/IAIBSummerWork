(* Task 4: lexer for the calculator grammar *)

type token =
  | NUMBER of float
  | PLUS
  | MINUS
  | POW
  | COS
  | FAC
  | EOF

exception Lex_error of string

let string_of_token = function
  | NUMBER f -> "NUMBER(" ^ string_of_float f ^ ")"
  | PLUS -> "PLUS"
  | MINUS -> "MINUS"
  | POW -> "POW"
  | COS -> "COS"
  | FAC -> "FAC"
  | EOF -> "EOF"

let is_digit c = c >= '0' && c <= '9'

(* unsigned int, no leading zeros *)
let scan_uint s pos =
  let len = String.length s in
  if pos >= len || not (is_digit s.[pos]) then
    raise (Lex_error "expected a digit");
  if s.[pos] = '0' then begin
    if pos + 1 < len && is_digit s.[pos + 1] then
      raise (Lex_error "leading zero not allowed");
    ("0", pos + 1)
  end else begin
    let start = pos in
    let i = ref pos in
    while !i < len && is_digit s.[!i] do incr i done;
    (String.sub s start (!i - start), !i)
  end

(* any run of digits, i.e. fractional part *)
let scan_digits s pos =
  let len = String.length s in
  if pos >= len || not (is_digit s.[pos]) then
    raise (Lex_error "expected a digit");
  let start = pos in
  let i = ref pos in
  while !i < len && is_digit s.[!i] do incr i done;
  (String.sub s start (!i - start), !i)

(* M | M DOT N | M DOT | DOT N *)
let scan_unsigned_float s pos =
  let len = String.length s in
  if pos < len && s.[pos] = '.' then
    let (frac, pos2) = scan_digits s (pos + 1) in
    ("." ^ frac, pos2)
  else begin
    let (intpart, pos1) = scan_uint s pos in
    if pos1 < len && s.[pos1] = '.' then begin
      if pos1 + 1 < len && is_digit s.[pos1 + 1] then
        let (frac, pos2) = scan_digits s (pos1 + 1) in
        (intpart ^ "." ^ frac, pos2)
      else
        (intpart ^ ".", pos1 + 1)
    end else
      (intpart, pos1)
  end

let sign_str c = if c = '+' then "+" else "-"

(* PLUS M | MINUS M | M, for the exponent *)
let scan_signed_int s pos =
  let len = String.length s in
  if pos < len && (s.[pos] = '+' || s.[pos] = '-') then
    let (digits, pos1) = scan_uint s (pos + 1) in
    (sign_str s.[pos] ^ digits, pos1)
  else
    scan_uint s pos

(* F, optionally followed by an exponent *)
let scan_number_body s pos =
  let len = String.length s in
  let (fpart, pos1) = scan_unsigned_float s pos in
  if pos1 < len && s.[pos1] = 'e' then
    let (exp_str, pos2) = scan_signed_int s (pos1 + 1) in
    (fpart ^ "e" ^ exp_str, pos2)
  else
    (fpart, pos1)

(* decide if a +/- starts a signed number or is a binary operator,
   based on the previous token *)
let expects_operand = function
  | None -> true
  | Some (PLUS | MINUS | POW | COS) -> true
  | Some (NUMBER _ | FAC | EOF) -> false

let tokenize (s : string) : token list =
  let len = String.length s in
  let tokens = ref [] in
  let pos = ref 0 in
  let prev = ref None in
  let push t = tokens := t :: !tokens; prev := Some t in
  while !pos < len do
    let c = s.[!pos] in
    if c = ' ' || c = '\t' || c = '\n' || c = '\r' then
      incr pos
    else if c = '^' then (push POW; incr pos)
    else if c = '!' then (push FAC; incr pos)
    else if c = 'c' && !pos + 3 <= len && String.sub s !pos 3 = "cos" then
      (push COS; pos := !pos + 3)
    else if is_digit c || c = '.' then begin
      let (numstr, pos1) = scan_number_body s !pos in
      push (NUMBER (float_of_string numstr));
      pos := pos1
    end
    else if c = '+' || c = '-' then begin
      if expects_operand !prev then begin
        let after_sign = !pos + 1 in
        if after_sign >= len
        || not (is_digit s.[after_sign] || s.[after_sign] = '.')
        then raise (Lex_error "expected a number after sign");
        let (numstr, pos1) = scan_number_body s after_sign in
        push (NUMBER (float_of_string (sign_str c ^ numstr)));
        pos := pos1
      end else begin
        push (if c = '+' then PLUS else MINUS);
        incr pos
      end
    end
    else
      raise (Lex_error "unexpected character")
  done;
  List.rev (EOF :: !tokens)

let tokenize_to_array s = Array.of_list (tokenize s)

let () =
  let show s =
    print_string s;
    print_string " -> ";
    (try
       let toks = tokenize s in
       print_string (String.concat " " (List.map string_of_token toks))
     with Lex_error msg -> print_string ("LEX ERROR: " ^ msg));
    print_newline ()
  in
  show "2.3+4";
  show "-3.5e-2 ^ cos 0 !";
  show "0.5 + .5 + 5.";
  show "007";
  show "cos-3!";
  show "1e+10 - 2e03"
