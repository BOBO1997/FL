open Syntax
open Eval

exception Exited_Successfully

let rec print_result env lst =
    (*print_env env;*)
    (*print_string "\n";*)
    match lst with
    | [] -> env
    | y::yd -> let (id, newenv, v) = y in
    (Printf.printf "%s = " id;
	 print_value v;
	 print_newline ();
	 print_result newenv yd)

let rec read_eval_print env =
	print_string "# ";
	flush stdout;
	let cmd =
        (try
            Parser.toplevel Lexer.main (Lexing.from_channel stdin)
         with Parsing.Parse_error -> (print_string "Syntax Error\n"; read_eval_print env))
    in
    try
        read_eval_print (print_result env (eval_command env cmd))
    with 
    | EvalErr -> (print_string "Evaluation Error\n";read_eval_print env)
    | QUIT -> (print_string "Exited."; raise Exited_Successfully)

let initial_env =
	extend "i" (VInt 1)
	    (extend "v" (VInt 5)
		    (extend "x" (VInt 10)
			    empty_env))
		
let _ = read_eval_print empty_env
