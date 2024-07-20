
(*let create_token state tk lex lit =*)
  (*let {start; source; tokens; current; line} = state in*)
  (*let new_token = {kind= tk; lexeme= lex; literal= lit} in*)
  (*let new_tokens = tokens @ [new_token] in*)
  (*{start; source; tokens= new_tokens; current; line}*)

(*let create_token state tk =*)
  (*let lex = String.sub state.source state.start state.current in*)
  (*create_token state tk lex (String_Literal lex)*)

(*let scan_token state character =*)
  (*match character with*)
  (*| '(' ->*)
    (*create_token state Left_Paren*)
  (*| ')' ->*)
    (*create_token state Right_Paren*)
  (*| _ ->*)
    (*state*)

let say_hi = print_endline "saying hi"
