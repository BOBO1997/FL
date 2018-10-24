type 'a m = 'a * string

(** (>>=) : 'a m -> ('a -> 'b m) -> 'b m *)
let (>>=) (x : 'a m) (f : 'a -> 'b m) =
    match x with
    | (a, b) -> match (f a) with | (c, d) -> (c, b^d)

(** return : 'a -> 'a m *)
let return (x : 'a) = (x, "")

(** writer : string -> unit m *)
let writer (m : string) = ((), m)

let msg n = ("Fib(" ^ (string_of_int n) ^")\n")
		
(** fib : int -> int m *)
let rec fib n =
	(writer (msg n)) >>= (fun _ ->
	if n <= 1 then
		return n
	else
		(fib (n-2)) >>= (fun x ->
		(fib (n-1)) >>= (fun y ->
		return (x + y))))
				 
let _ =
	let (_, m) = fib 4 in
	print_string m
(*
let f x =
    (x + 1, "call f("^(string_of_int x)^"), ")

let g x =
    (2 * x, "call g("^(string_of_int x)^"), ")

(f 3) >>= (fun a ->
(g a) >>= (fun b ->
(f b) >>= (fun c ->
    return c)))
*)
(** Expected Output:
	Fib(4)
	Fib(2)
	Fib(0)
	Fib(1)
	Fib(3)
	Fib(1)
	Fib(2)
	Fib(0)
	Fib(1)
 *)
