(* Definition of "the" list monad *)
type 'a m = 'a list

(** (>>=) : 'a m -> ('a -> 'b m) -> 'b m *)
let (>>=) (x : 'a m) (f : 'a -> 'b m) =
	List.concat (List.map f x)

(** return : 'a -> 'a m *)
let return (x : 'a) = [x]

(** guard : bool -> unit m *)
let guard (x : bool) =
	if x then return () else []

(** check if "banana + banana = sinamon" *)
let test_banana ba na si mo n =
	(100 * ba + 10 * na + na
	 + 100 * ba + 10 * na + na
	 = 1000 * si + 100 * na + 10 * mo + n)

(** check if "send + more = money" *)
let test_money s e n d m o r y =
	(1000 * s + 100 * e + 10 * n + d
	 + 1000 * m + 100 * o + 10 * r + e
	 = 10000 * m + 1000 * o + 100 * n + 10 * e + y)

let find_banana =
    [1; 2; 3; 4; 5; 6; 7; 8; 9; 0] >>=           (fun x1 ->
    [1; 2; 3; 4; 5; 6; 7; 8; 9; 0] >>=           (fun x2 ->
    [1; 2; 3; 4; 5; 6; 7; 8; 9; 0] >>=           (fun x3 ->
    [1; 2; 3; 4; 5; 6; 7; 8; 9; 0] >>=           (fun x4 ->
    [1; 2; 3; 4; 5; 6; 7; 8; 9; 0] >>=           (fun x5 ->
    (guard (test_banana x1 x2 x3 x4 x5)) >>= (fun _ ->
    return (x1, x2, x3, x4, x5)))))))

let rec remove x lst =
    match lst with
    | [] -> []
    | y::yd -> if x = y then yd else y::(remove x yd)

let find_money =
    let lst1 = [1; 2; 3; 4; 5; 6; 7; 8; 9; 0] in lst1 >>= (fun x1 ->
    let lst2 = remove x1 lst1 in lst2 >>=                 (fun x2 ->
    let lst3 = remove x2 lst2 in lst3 >>=                 (fun x3 ->
    let lst4 = remove x3 lst3 in lst4 >>=                 (fun x4 ->
    let lst5 = remove x4 lst4 in lst5 >>=                 (fun x5 ->
    let lst6 = remove x5 lst5 in lst6 >>=                 (fun x6 ->
    let lst7 = remove x6 lst6 in lst7 >>=                 (fun x7 ->
    let lst8 = remove x7 lst7 in lst8 >>=                 (fun x8 ->
    (guard ((x1 <> 0) && (x5 <> 0) && (test_money x1 x2 x3 x4 x5 x6 x7 x8))) >>= (fun _ ->
    return (x1, x2, x3, x4, x5, x6, x7, x8))))))))))

