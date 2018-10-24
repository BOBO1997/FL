type infInt = VInt of int | INF

module type SEMIRING = sig
    type t
    val add : t -> t -> t
    val mul : t -> t -> t
    val unit : t
    val zero : t
end

module SemiInt =
    (*functor (T: SEMIRING) ->*) struct
        type t = infInt
        let add a b = match a with
                        | INF -> INF
                        | VInt p -> (match b with
                                    | INF -> INF
                                    | VInt q -> VInt (p + q))
        let mul a b = match a with
                        | INF -> INF
                        | VInt p -> (match b with
                                    | INF -> INF
                                    | VInt q -> VInt (p * q))
        let unit = VInt 1
        let zero = VInt 0
    end

module SemiBool =
    (*functor (T: SEMIRING) ->*) struct
        type t = bool
        let add a b = a || b
        let mul a b = a && b
        let unit = true
        let zero = false
    end

exception Different_Length

module type MATRIX =
    functor (T: SEMIRING) -> sig
        type matrix = T.t list list
        val add : matrix -> matrix -> matrix
        val mul : matrix -> matrix -> matrix
    end

module Matrix : MATRIX =
    functor (T: SEMIRING) -> struct
        type matrix = T.t list list
        let rec add m1 m2 = 
            let rec addList l1 l2 =
                match l1 with
                | [] -> (match l2 with
                            | [] -> []
                            | b::bd -> raise Different_Length)
                | a::ad -> (match l2 with
                            | [] -> raise Different_Length
                            | b::bd -> (T.add a b)::(addList ad bd))
            in match m1 with
            | [] -> (match m2 with
                        | [] -> []
                        | b::bd -> raise Different_Length)
            | a::ad -> (match m2 with
                        | [] -> raise Different_Length
                        | b::bd -> (addList a b)::(add ad bd))
        let rec mul m1 m2 =
            (*行列の先頭行を取り出してくれる、1次元listを返す*)
            let rec getHvec matrix =
                match matrix with
                | [] -> []
                | a::ad -> (match a with
                            | [] -> []
                            | b::bd -> b::(getHvec ad))
            in let rec getRmat matrix =
                match matrix with
                | [] -> matrix
                | a::ad -> (match a with
                            | [] -> matrix
                            | b::bd -> (match bd with
                                        | [] -> []
                                        | c::cd -> bd::(getRmat ad)))
            (*抜き出した行ベクトルと列ベクトルの内積の計算、値を返してくれる*)
            in let rec dot l1 l2 =
                match l1 with
                | [] -> (match l2 with
                            | [] -> T.zero
                            | b::bd -> raise Different_Length)
                | a::ad -> (match l2 with
                            | [] -> raise Different_Length
                            | b::bd -> T.add (T.mul a b) (dot ad bd))
            (*getHvecで抜き出した列ベクトルに対してm1に対してそれぞれ内積を取り、2次元listを返してくれる*)
            in let rec mulList matrix lst =
                match matrix with
                | [] -> []
                | a::ad -> [(dot a lst)]::(mulList ad lst)
            (*mulListで得られた2次元listを答えとなるmatrixに編入*)
            in let rec appendEachOther t_dim matrix =
                match matrix with
                | [] -> (match t_dim with
                            | [] -> []
                            | b::bd -> t_dim)
                | a::ad -> (match t_dim with
                            | [] -> raise Different_Length
                            | b::bd -> (b@a)::(appendEachOther bd ad))
            (*finalFuncは答えを返す、2次元listを返す*)
            in let rec finalFunc hlist rmatrix =
                match rmatrix with
                | [] -> mulList m1 hlist
                | a::ad -> (match a with
                            | [] -> []
                            | b::bd -> (appendEachOther (mulList m1 hlist) (finalFunc (getHvec rmatrix) (getRmat rmatrix))))
            in finalFunc (getHvec m2) (getRmat m2)
    end

module IntMatrix = Matrix (SemiInt)
module BoolMatrix = Matrix (SemiBool)

