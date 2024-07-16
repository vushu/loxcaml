type token_kind = 
  | Left_Paren
  | Right_Paren
  | Left_Brace
  | Right_Brace
  | Comma
  | Dot
  | Minus
  | Plus
  | Semicolon
  | Slash
  | Star
  | Bang
  | Bang_Equal
  | Equal
  | Equal_Equal
  | Greater
  | Greater_Equal
  | Less
  | Less_Equal
  | Identifier
  | String
  | Number
  | And
  | Or
  | Class
  | Else
  | False
  | Fun
  | For
  | If
  | Nil
  | Print
  | Return
  | Super
  | This
  | True
  | Var
  | While
  | EOF

type literal = 
  | String of string | Float of float | Int of int | Bool of bool

type token = { kind : token_kind; lexeme : string; literal : literal}

