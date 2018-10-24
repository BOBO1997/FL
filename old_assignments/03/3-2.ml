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

exception Empty

module MyStack : SigMyStack =
	functor (T : ordered_type) ->
	struct
		type t = T.t list
		let empty = []
		let push item stack = item::stack
		let pop stack =
			match stack with
			| [] -> raise Empty (*raise No_Items_in_Stack (*仮置き*)*)
			| a::ay -> (a, ay)
		let push item stack = item::stack
		let rec size stack =
			match stack with
			| [] -> 0
			| y::yd -> 1 + size yd
	end

module OrderedInt = 
struct
    type t = int
end

module IntStack = MyStack (OrderedInt)
