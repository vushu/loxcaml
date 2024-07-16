open Tokens
type scanner_state = { start : int; source : string; tokens : token list; current : int; line : int }




let create_token state tk lit =  
  let { start; source; tokens; current; line} = state in
  let new_token = { kind = tk; lexeme = String.sub source start current; literal = lit } in
  let new_tokens = tokens@[new_token] in
  {start = start; source = source; tokens = new_tokens; current = current; line = line}
 

let scan_token state character = 

  match character with
  | '(' -> create_token state Left_Paren 
  | ')' ->  create_token state Right_Paren
