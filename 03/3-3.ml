type order = LT | EQ | GT

(*signiture*)
module type ORDERED_TYPE =
sig
	type t
	val compare : t -> t -> order
end

(*signiture in functor*)
module type MULTISET2 =
	functor (T : ORDERED_TYPE) -> 
		sig 
			type t
			val empty  : t
			val add	   : T.t -> t -> t
			val remove : T.t -> t -> t
			val count  : T.t -> t -> int
		end

exception No_Such_Value
exception Remove_Error

(*struct in functor*)
module Multiset2 : MULTISET2 =
	functor (T : ORDERED_TYPE) -> struct
		type t = Nil | Node of T.t * t * t
		let empty = Nil
		let rec remove value tree =
			let rec searchmin ptree =
				match ptree with
				| Nil -> raise Remove_Error
				| Node (a, l, r) -> if l = Nil then a else (searchmin l)
			in match tree with
				| Nil -> raise No_Such_Value
				| Node (a, l, r) -> 
					(match T.compare value a with 
					| LT -> Node (a, remove value l, r)
					| GT -> Node (a, l, remove value r)
					| EQ -> (match r with
							| Nil -> (match l with
										| Nil -> Nil
										| Node (c, lc, rc) -> Node (c, lc, rc))
							| Node (b, lb, rb) -> (match l with
													| Nil -> Node (b, lb, rb)
													| Node (d, ld, rd) ->
														(if b = value
														then Node (a, l, remove value r)
														else if lb = Nil then if rb = Nil then Node (b, l, Nil)
																				else Node (searchmin rb, Nil, remove (searchmin rb) rb)
																else Node (searchmin l, remove (searchmin l) l, r)))))
		let rec add value tree =
			match tree with
			| Nil -> Node (value , Nil, Nil)
			| Node (a, l, r) -> 
				match T.compare value a with
					| LT -> Node (a, add value l, r)
					| GT -> Node (a, l, add value r)
					| EQ -> Node (a, l, add value r)

		let rec count value tree =
			match tree with
			| Nil -> 0
			| Node (a, l, r) -> match T.compare value a with
								| LT -> count value l
								| GT -> count value r
								| EQ -> 1 + (count value r)
	end

(*struct*)
module OrderedString =
struct
	type t = string
	let compare x y = 
		let r = Pervasives.compare x y in
			if r > 0 then GT 
			else if r < 0 then LT 
			else EQ
end 

module StringMultiset = Multiset2 (OrderedString)
