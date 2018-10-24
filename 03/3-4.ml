type order = LT | EQ | GT

(*signiture*)
module type ORDERED_TYPE =
	sig
		type key
		type value
		val compare : key -> key -> order
	end

(*
module type AssocTuple =
	functor (T: ORDERED_TYPE) ->
		sig
			type {key: T.t ; value: T.t}
		end
*)

(*signiture in functor*)
module type SigAssoc =
	functor (T : ORDERED_TYPE) -> 
		sig 
			type t
			val empty  : t
			val add	   : T.key -> T.value -> t -> t
			val remove : T.key -> t -> t
			val lookup : T.key -> t -> T.value
		end

exception No_Such_Key
exception Remove_Error

(*struct in functor*)
module StrAssoc : SigAssoc =
	functor (T : ORDERED_TYPE) -> struct
		type t = Nil | Node of (T.key * T.value) * t * t
		let empty = Nil
		let rec remove value tree =
			(*部分木の中で最小のkey(最も左のkey)を抜き出す関数*)
			let rec searchmin ptree =
				match ptree with
				| Nil -> raise Remove_Error
				| Node ((k, v), l, r) -> if l = Nil then (k, v) else (searchmin l)
			in match tree with
				| Nil -> raise No_Such_Key (*葉まで辿っても削除対象が見つからなかった場合*)
				| Node ((k, v), l, r) -> 
					match T.compare value k with
					| LT -> Node ((k, v), remove value l, r) (*比較したノードより小さかった場合*)
					| GT -> Node ((k, v), l, remove value r) (*比較したノードより大きかった場合*)
					| EQ -> (match r with 
							| Nil -> (match l with 
										| Nil -> Nil (*右の要素も左の要素もなかった場合*)
										| Node ((kc, vc), lc, rc) -> Node ((kc, vc), lc, rc)) (*左の要素しかなかった場合*)
							| Node ((kb, vb), lb, rb) -> (match l with
													| Nil -> Node ((kb, vb), lb, rb) (*右の要素しかなかった場合*)
													| Node ((kd, vd), ld, rd) -> (*右も左も要素がある場合*)
											    	    Node (searchmin r, l,  remove (match (searchmin r) with | (keey, valuee) -> keey) r))) (*子が違うkeyの時に、欠けた部分に子の左の要素の最小値を入れる*)
		let rec add key value tree =
			match tree with
			| Nil -> Node ((key, value) , Nil, Nil)
			| Node ((k, v), l, r) -> 
				match T.compare key k with
					| LT -> Node ((k, v), add key value l, r)
					| GT -> Node ((k, v), l, add key value r)
					| EQ -> Node ((key, value), l, r)
		
		let rec lookup key tree =
			match tree with
			| Nil -> raise No_Such_Key
			| Node ((k, v), l, r) ->
				match T.compare key k with
					| LT -> lookup key l
					| GT -> lookup key r
					| EQ -> v
			
	end

(*struct*)
module OrderedString =
	struct
		type key = string
		type value = string
		let compare x y =
			let r = Pervasives.compare x y in
				if      r > 0 then GT
				else if r < 0 then LT
				else               EQ
	end

module StringAssoc = StrAssoc (OrderedString)
