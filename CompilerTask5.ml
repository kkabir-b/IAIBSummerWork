
type tree =
  | Num of float
  | Plus of tree * tree
  | Minus of tree * tree
  | Pow of tree * tree
  | Cos of tree
  | Fact of tree

let rec print_tree t =
  match t with
  | Num f -> print_float f
  | Plus (a, b) ->
      print_string "(";
      print_tree a;
      print_string " + ";
      print_tree b;
      print_string ")"
  | Minus (a, b) ->
      print_string "(";
      print_tree a;
      print_string " - ";
      print_tree b;
      print_string ")"
  | Pow (a, b) ->
      print_string "(";
      print_tree a;
      print_string " ^ ";
      print_tree b;
      print_string ")"
  | Cos a ->
      print_string "cos(";
      print_tree a;
      print_string ")"
  | Fact a ->
      print_tree a;
      print_string "!"

exception Parse_error of string

type term = TNUMBER | TPLUS | TMINUS | TPOW | TCOS | TFAC | TEOF

let term_index t =
  match t with
  | TNUMBER -> 0 | TPLUS -> 1 | TMINUS -> 2
  | TPOW -> 3 | TCOS -> 4 | TFAC -> 5 | TEOF -> 6

let num_terms = 7

let term_of_token tok =
  match tok with
  | NUMBER _ -> TNUMBER
  | PLUS -> TPLUS
  | MINUS -> TMINUS
  | POW -> TPOW
  | COS -> TCOS
  | FAC -> TFAC
  | EOF -> TEOF

type nonterm = NA | NS | NP | NC | NG

let nonterm_index nt =
  match nt with
  | NA -> 0 | NS -> 1 | NP -> 2 | NC -> 3 | NG -> 4

let num_nonterms = 5
let num_states = 16


let productions : (nonterm * int) array =
  [|
    (NA, 1);   (* 0: A' -> A   (augmenting, never reduced) *)
    (NA, 1);   (* 1: A -> S *)
    (NA, 3);   (* 2: A -> A PLUS S *)
    (NS, 1);   (* 3: S -> P *)
    (NS, 3);   (* 4: S -> S MINUS P *)
    (NP, 1);   (* 5: P -> C *)
    (NP, 3);   (* 6: P -> C POW P *)
    (NC, 1);   (* 7: C -> G *)
    (NC, 2);   (* 8: C -> COS C *)
    (NG, 1);   (* 9: G -> NUMBER *)
    (NG, 2);   (* 10: G -> G FAC *)
  |]

type action = Shift of int | Reduce of int | Accept | Error

let action_tbl = Array.make_matrix num_states num_terms Error
let goto_tbl = Array.make_matrix num_states num_nonterms (-1)

let set_action state t a = action_tbl.(state).(term_index t) <- a
let set_goto state nt s = goto_tbl.(state).(nonterm_index nt) <- s

let () =
  (* shifts *)
  set_action 0 TNUMBER (Shift 7);   set_action 0 TCOS (Shift 6);
  set_action 1 TPLUS   (Shift 8);
  set_action 2 TMINUS  (Shift 10);
  set_action 4 TPOW    (Shift 12);
  set_action 5 TFAC    (Shift 14);
  set_action 6 TNUMBER (Shift 7);   set_action 6 TCOS (Shift 6);
  set_action 8 TNUMBER (Shift 7);   set_action 8 TCOS (Shift 6);
  set_action 9 TMINUS  (Shift 10);
  set_action 10 TNUMBER (Shift 7);  set_action 10 TCOS (Shift 6);
  set_action 12 TNUMBER (Shift 7);  set_action 12 TCOS (Shift 6);

  set_action 1 TEOF Accept;

  (* reduces *)
  set_action 2 TPLUS (Reduce 1); set_action 2 TEOF (Reduce 1);
  set_action 3 TPLUS (Reduce 3); set_action 3 TMINUS (Reduce 3); set_action 3 TEOF (Reduce 3);
  set_action 4 TPLUS (Reduce 5); set_action 4 TMINUS (Reduce 5); set_action 4 TEOF (Reduce 5);
  set_action 5 TPLUS (Reduce 7); set_action 5 TMINUS (Reduce 7);
  set_action 5 TPOW  (Reduce 7); set_action 5 TEOF   (Reduce 7);
  set_action 7 TPLUS (Reduce 9); set_action 7 TMINUS (Reduce 9);
  set_action 7 TPOW  (Reduce 9); set_action 7 TFAC   (Reduce 9); set_action 7 TEOF (Reduce 9);
  set_action 9 TPLUS (Reduce 2); set_action 9 TEOF (Reduce 2);
  set_action 11 TPLUS (Reduce 4); set_action 11 TMINUS (Reduce 4); set_action 11 TEOF (Reduce 4);
  set_action 13 TPLUS (Reduce 6); set_action 13 TMINUS (Reduce 6); set_action 13 TEOF (Reduce 6);
  set_action 14 TPLUS (Reduce 10); set_action 14 TMINUS (Reduce 10);
  set_action 14 TPOW  (Reduce 10); set_action 14 TFAC   (Reduce 10); set_action 14 TEOF (Reduce 10);
  set_action 15 TPLUS (Reduce 8); set_action 15 TMINUS (Reduce 8);
  set_action 15 TPOW  (Reduce 8); set_action 15 TEOF   (Reduce 8);

  (* goto *)
  set_goto 0 NA 1; set_goto 0 NS 2; set_goto 0 NP 3; set_goto 0 NC 4; set_goto 0 NG 5;
  set_goto 6 NC 15; set_goto 6 NG 5;
  set_goto 8 NS 9; set_goto 8 NP 3; set_goto 8 NC 4; set_goto 8 NG 5;
  set_goto 10 NP 11; set_goto 10 NC 4; set_goto 10 NG 5;
  set_goto 12 NP 13; set_goto 12 NC 4; set_goto 12 NG 5


type slot = STree of tree | SOp

(* build the tree for production p from its popped children, in order *)
let build_tree p children =
  match p, children with
  | 1, [ STree s ] -> s                                  (* A -> S *)
  | 2, [ STree a; SOp; STree s ] -> Plus (a, s)           (* A -> A PLUS S *)
  | 3, [ STree p1 ] -> p1                                 (* S -> P *)
  | 4, [ STree s; SOp; STree p1 ] -> Minus (s, p1)        (* S -> S MINUS P *)
  | 5, [ STree c ] -> c                                   (* P -> C *)
  | 6, [ STree c; SOp; STree p1 ] -> Pow (c, p1)          (* P -> C POW P *)
  | 7, [ STree g ] -> g                                   (* C -> G *)
  | 8, [ SOp; STree c ] -> Cos c                          (* C -> COS C *)
  | 9, [ STree n ] -> n                                   (* G -> NUMBER *)
  | 10, [ STree g; SOp ] -> Fact g                        (* G -> G FAC *)
  | _ -> raise (Parse_error "malformed production during reduce")

let parse (tokens : token array) : tree =
  let state_stack = ref [ 0 ] in
  let value_stack = ref [] in
  let pos = ref 0 in
  let cur_state () = List.hd !state_stack in
  let rec loop () =
    let tok = tokens.(!pos) in
    let t = term_of_token tok in
    match action_tbl.(cur_state ()).(term_index t) with
    | Shift s ->
        let slot =
          match tok with
          | NUMBER f -> STree (Num f)
          | _ -> SOp
        in
        state_stack := s :: !state_stack;
        value_stack := slot :: !value_stack;
        pos := !pos + 1;
        loop ()
    | Reduce p ->
        let nt, len = productions.(p) in
        let children = ref [] in
        for _ = 1 to len do
          (match !value_stack with
          | h :: rest -> children := h :: !children; value_stack := rest
          | [] -> raise (Parse_error "stack underflow"));
          (match !state_stack with
          | _ :: rest -> state_stack := rest
          | [] -> raise (Parse_error "stack underflow"))
        done;
        let result = build_tree p !children in
        let s' = cur_state () in
        let s'' = goto_tbl.(s').(nonterm_index nt) in
        if s'' = -1 then raise (Parse_error "no GOTO entry")
        else begin
          state_stack := s'' :: !state_stack;
          value_stack := STree result :: !value_stack;
          loop ()
        end
    | Accept -> (
        match !value_stack with
        | [ STree t ] -> t
        | _ -> raise (Parse_error "bad stack at accept"))
    | Error -> raise (Parse_error "unexpected token")
  in
  loop ()

let parse_string (s : string) : tree = parse (tokenize_to_array s)

let () =
  let show s =
    (try
       let t = parse_string s in
       print_tree t;
       print_newline ()
     with
    | Lex_error msg -> print_string "LEX ERROR: "; print_string msg; print_newline ()
    | Parse_error msg -> print_string "PARSE ERROR: "; print_string msg; print_newline ())
  in
  show "2.3+4";
  show "-3.5e-2 ^ cos 0 !";
  show "0.5 + .5 + 5.";
  show "2^3^2";
  show "1-2-3"
