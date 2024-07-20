open Scanner_state
open Tokens
let add_token state tk lexeme lit =
  let {start; source; tokens; current; line} = state in
  let new_token = {kind= tk; lexeme; literal= lit} in
  let new_tokens = tokens @ [new_token] in
  {start; source; tokens= new_tokens; current; line}

let add_token state tk =
  let lex = String.sub state.source state.start state.current in
  add_token state tk lex (String_Literal lex)

let advance {start; source; tokens; current; line} = 
  let inc = current + 1 in
  {start; source; tokens; current = inc; line;}
let increment_line {start; source; tokens; current; line} = 
  {start; source; tokens; current;  line = line + 1;}

let is_at_end state = state.current > String.length state.source

let matches state c = 
  if is_at_end state then None
  else if (String.get state.source state.current) <> c then None
  else 
    Some (advance state)
let peek state = String.get state.source (state.current + 1)

let rec skipping_loop state =
  if peek state <> '\n' && not (is_at_end state) then
    skipping_loop (advance state)
  else state

 let scan_token state  = 
  let s = advance state in
  let c = String.get s.source s.current in
  match c with
    | '(' ->  add_token s Left_Paren
    | ')' ->  add_token s Right_Paren
    | '{' ->  add_token s Left_Brace
    | '}' ->  add_token s Right_Brace
    | ',' ->  add_token s Comma
    | '.' ->  add_token s Dot
    | '-' ->  add_token s Minus
    | '+' ->  add_token s Plus
    | '*' ->  add_token s Star

    | '!' ->  (match matches s '=' with
              | Some ns -> add_token ns Bang_Equal
              | None -> add_token s Bang)

    | '=' ->  (match matches s '=' with
              | Some ns -> add_token ns Equal_Equal
              | None -> add_token s Equal)

    | '<' ->  (match matches s '=' with
              | Some ns -> add_token ns Less_Equal
              | None -> add_token s Less)
    | '>' ->  (match matches s '=' with
              | Some ns -> add_token ns Greater_Equal
              | None -> add_token s Greater)

    | '/' -> 
      (match matches s '/' with
              | Some ns -> skipping_loop ns
              | None  -> add_token s Slash)

    | ' ' | '\r' | '\t' -> state (* Ignoring whitespaces *)
    | '\n' -> increment_line state
    | '"' -> state
    | _ -> state


let say_hi = print_endline "saying hi"
let rec scan_token_loop state = 
    if not (is_at_end state) then 
      let {start = _; source; tokens; current; line} = state in
      scan_token {start = current; source; tokens; current; line }
      |> scan_token_loop
    else state

let scan_tokens code : token list = 
  let state : scanner_state = { start = 0; source = code; tokens = []; current = 0 ; line = 0 } in
  let new_state = scan_token_loop state in
  new_state.tokens
