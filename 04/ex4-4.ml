type 'a option = Some of 'a | None

type 'a m = ('a * 'a) list -> ('a * ('a * 'a) list)

let rec search n memory =
    match memory with
    | [] -> None
    | y::yd -> let (k, v) = y in if n = k then Some v else search n yd

(** (>>=) : 'a m -> ('a -> 'b m) -> 'b m *)
let (>>=) (x : 'a m) (f : 'a -> 'b m) =
    fun memory -> let (a, m) = x memory in (f a) m

(** return : 'a -> 'a m *)
let return x = fun memory -> (x, memory)

(** memo : (int -> int m) -> int -> int m *)
let memo (f : int -> int m) n =
    fun memory -> match search n memory with
                  | None -> let (v, m) = (f n) memory in (v, (n, v)::m)
                  | Some v -> (v, memory)

(** runMemo : 'a m -> 'a *)
let runMemo (x : 'a m) = let (a, memory) = x [] in a

let rec fib n =
    if n <= 1 then
        return n
    else
        (memo fib (n-2)) >>= (fun r1 ->
        (memo fib (n-1)) >>= (fun r2 ->
            return (r1 + r2)))
			   
let _ =
    if runMemo (fib 80) = 23416728348467685 && runMemo (fib 10) = 55 then
        print_string "ok\n"
    else
        print_string "wrong\n"
