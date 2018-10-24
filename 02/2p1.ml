type nat = Z | S of nat;;

let rec add a b =
	match a with
	| Z -> b
	| S c -> add c (S b);; 

let rec sub a b =
	match a with
	| Z -> Z
	| S c -> match b with
			| Z -> a
			| S d -> sub c d;;

let rec mul a b =
	match a with
	| Z -> Z
	| S c -> add b (mul c b);;

let rec pow a b =
	match b with
	| Z -> (S Z)
	| S d -> mul a (pow a d);;

let rec fact a =
	match a with
	| Z -> (S Z)
	| S d -> mul a (fact d);;

let rec n2i a =
	match a with
	| Z -> 0
	| S c -> 1 + n2i c;;

let rec i2n a =
	if a = 0 then Z else S (i2n (a - 1));;
