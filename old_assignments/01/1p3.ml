let rec fix f x = f (fix f) x;;

let sum_to n = 
	let sub_sum f n = if n = 0 then 0 else n + f (n - 1)
	in fix sub_sum n;;

let is_prime n =
	let sub_prime f n x =
		if x = n then true
		else if (n mod x) = 0 then false
			else f n (x + 1)
	in fix sub_prime n 2;;
 
let gcd x y = 
	let sub_gcd f x y = if (x mod y) = 0 then y else f y (x mod y)
    in if x > y then fix sub_gcd x y
    else fix sub_gcd y x;;

