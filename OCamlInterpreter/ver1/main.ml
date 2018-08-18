open Syntax
open TySyntax
open Infer
open Eval

let rec read_eval_print tyenv env =
  print_string "# ";
  flush stdout;
  try
  let cmd = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
  let (ty, new_tyenv) = infer_cmd tyenv cmd in
  let (id, newenv, v) = eval_command env cmd in
  (Printf.printf "%s : " id;
   print_string " = ";
   print_value v;
   print_newline ();
   read_eval_print new_tyenv newenv)
  with
  | Parsing.Parse_error -> print_string "Parsing Error...\n"; read_eval_print tyenv env
  | MatchErr -> read_eval_print tyenv env
  | TyError -> print_string "Type Error...\n"; read_eval_print tyenv env
  | EvalErr -> print_string "Eval Error...\n"; read_eval_print tyenv env
  | Failure str -> print_string "Lexing Failure...\n"; read_eval_print tyenv env

let _ = read_eval_print empty_tyenv empty_env
