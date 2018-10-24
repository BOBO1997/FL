module type ordered_type =
sig
	type t
end

module type SigMyStack =
	functor (T : ordered_type) ->
		sig
			type t
			val empty : t
			val push : T.t -> t -> t
			val pop : t -> (T.t * t)
			val size : t -> int
		end



