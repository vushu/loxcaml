open Tokens
type scanner_state = { start : int; source : string; tokens : token list; current : int; line : int }