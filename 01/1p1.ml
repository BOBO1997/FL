let rec sum_to n =	
	if n = 0 then 0
	else n + sum_to (n - 1);;

let is_prime n =
	let rec sub_prime n x =
		if x = n then true
		else if (n mod x) = 0 then false
			else sub_prime n (x + 1)
	in sub_prime n 2;;

let gcd x y =
	let rec sub_gcd x y = 
		if (x mod y) = 0 then y
		else sub_gcd y (x mod y)
	in if x > y then sub_gcd x y else sub_gcd y x;;
