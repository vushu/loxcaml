(* open Loxcaml *)
open Loxcaml.Scanner
open Loxcaml.Tokens
let code  = "{}(),.+->=/"
let test_left_paren = scan_tokens code  |> List.iter (fun x -> print_endline x.lexeme)
let code_length = print_int (String.length code)

(* let mama = print_int (String.length "((") *)


(* let mama = (String.get "(" 0) |> print_char *)