open Array 
open Color 
open Command 

exception Error

(* (O, X) *)
let init_board () = ref (0x0000001008000000L, 0x0000000810000000L)

let nbit_subst0 bits n = let newbits = Int64.logand bits (Int64.lognot (Int64.shift_left 1L n)) in newbits

let nbit_subst1 bits n = let newbits = Int64.logor bits (Int64.shift_left 1L n) in newbits

let bitcount num =
    let num = Int64.add (Int64.logand num 0x5555555555555555L) (Int64.logand (Int64.shift_right_logical num 1) 0x5555555555555555L) in
    let num = Int64.add (Int64.logand num 0x3333333333333333L) (Int64.logand (Int64.shift_right_logical num 2) 0x3333333333333333L) in
    let num = Int64.add (Int64.logand num 0x0f0f0f0f0f0f0f0fL) (Int64.logand (Int64.shift_right_logical num 4) 0x0f0f0f0f0f0f0f0fL) in
    let num = Int64.add (Int64.logand num 0x00ff00ff00ff00ffL) (Int64.logand (Int64.shift_right_logical num 8) 0x00ff00ff00ff00ffL) in
    let num = Int64.add (Int64.logand num 0x0000ffff0000ffffL) (Int64.logand (Int64.shift_right_logical num 16) 0x0000ffff0000ffffL) in
    Int64.add (Int64.logand num 0x00000000ffffffffL) (Int64.logand (Int64.shift_right_logical num 32) 0x00000000ffffffffL)

(* 盤面をコピー *)
let copyBoard board =
    let newBoard = ref !board in newBoard

let countNone board =
    let (o, x) = !board in 64 - Int64.to_int (bitcount o) - Int64.to_int (bitcount x)

let dirs = [ (-1,-1); (0,-1); (1,-1); (-1,0); (1,0); (-1,1); (0,1); (1,1) ]

let isnbit1 bits n = (Int64.logand (Int64.shift_right_logical bits n) 1L) = 1L

let isColor (o, x) (i, j) color =
    if (i < 1) || (i > 8) || (j < 1) || (j > 8) then
        false
    else
        match color with
        | 1 -> isnbit1 o ((8 - i) + 8 * (8 - j))
        | 2 -> isnbit1 x ((8 - i) + 8 * (8 - j))
        | _ -> raise Error

let flippable_indices_line board color (di,dj) (i,j) =
  let ocolor = opposite_color color in
  let rec f (di,dj) (i,j) r =
    if isColor !board (i, j) ocolor then 
      g (di,dj) (i+di,j+dj) ( (i,j) :: r )
    else 
      [] 
  and    g (di,dj) (i,j) r =
    if isColor !board (i, j) ocolor then 
      g (di,dj) (i+di,j+dj) ( (i,j) :: r )
    else if isColor !board (i, j) color then 
      r
    else 
      [] in 
    f (di,dj) (i,j) []

(* (i, j)に石を置いたときにひっくり返せる場所 *)
let flippable_indices board color (i, j) =
    let bs = List.map (fun (di, dj) -> flippable_indices_line board color (di, dj) (i + di, j + dj)) dirs in 
        List.concat bs 
        
let is_effective board color (i,j) =
    match flippable_indices board color (i,j) with 
            [] -> false
        | _    -> true 

(* そのcellにコマを置けるかどうかのboolをかえす *)
let is_valid_move board color (i,j) =
    let (o, x) = !board in
    (Int64.logand (Int64.shift_right_logical o ((8 - i) + 8 * (8 - j))) 1L) = 0L && 
    (Int64.logand (Int64.shift_right_logical x ((8 - i) + 8 * (8 - j))) 1L) = 0L && 
    is_effective board color (i, j) 

let boardSubst board color (i, j) =
    let (o, x) = !board in
    match color with
    | 2 ->
        board := (nbit_subst0 o ((8 - i) + 8 * (8 - j)), nbit_subst1 x ((8 - i) + 8 * (8 - j)))
    | 1 ->
        board := (nbit_subst1 o ((8 - i) + 8 * (8 - j)), nbit_subst0 x ((8 - i) + 8 * (8 - j)))
    | _ -> raise Error

(* mv指令でひっくり返すべきcellをひっくり返す *)
let doMove board com color =
    match com with 
    | GiveUp -> board
    | Pass -> board
    | Mv (i,j) -> 
        let ms = flippable_indices board color (i, j) in
        let _    = List.map (boardSubst board color) ((i, j)::ms) in
        board 
    | _ -> board 

(* 座標生成 *)
let mix xs ys =
    List.concat (List.map (fun x -> List.map (fun y -> (x,y)) ys) xs)
	         
(* 置ける手の座標を羅列したlistを返す *)
let valid_moves board color = 
    let ls = [1;2;3;4;5;6;7;8] in 
    List.filter (is_valid_move board color)
        (mix ls ls) (* 全座標に対して、is_valid_moveがtrueになった座標のみを取り出している *)

let count board color =
    let (o, x) = !board in
    match color with
    | 2 -> Int64.to_int (bitcount x)
    | 1 -> Int64.to_int (bitcount o)
    | _ -> raise Error

(*************************************************************************************************************************************************)
(* 独自のmap関数 *)
(*************************************************************************************************************************************************)

let rec mymap f ms value (parentFlag, maximizeFlag) tailList funValue = (* maximizeFlagはこのmapで出したvalueのリストがmaximizeされるべきかどうかのflag *)
    match ms with
    | [] -> List.rev tailList
    | a::ay -> 
        let (i, j, return_value) = (f funValue) a in
        match (maximizeFlag, return_value, value, funValue) with
        | (true, Num rv, AnyValue, AnyValue) ->
            mymap f ay value (parentFlag, maximizeFlag) ((i, j, Num rv)::tailList) (Num rv)
        | (true, Num rv, AnyValue, Num funv) ->
            if rv > funv then mymap f ay value (parentFlag, maximizeFlag) ((i, j, Num rv)::tailList) (Num rv)
            else mymap f ay value (parentFlag, maximizeFlag) ((i, j, Num rv)::tailList) (Num funv) 
        | (true, Num rv, Num constr, AnyValue) -> 
            if parentFlag then
                if rv > constr then 
                    mymap f ay value (parentFlag, maximizeFlag) ((i, j, Num rv)::tailList) funValue 
                else 
                    mymap f ay value (parentFlag, maximizeFlag) ((0, 0, Invalid)::tailList) funValue
            else
                if rv < constr then mymap f ay value (parentFlag, maximizeFlag) ((i, j, Num rv)::tailList) funValue else []
        | (true, Num rv, Num constr, Num funv) -> 
            if parentFlag then
                if rv > constr then 
                    if rv > funv then mymap f ay value (parentFlag, maximizeFlag) ((i, j, Num rv)::tailList) (Num rv) 
                    else mymap f ay value (parentFlag, maximizeFlag) ((i, j, Num rv)::tailList) (Num funv) 
                else 
                    mymap f ay value (parentFlag, maximizeFlag) ((0, 0, Invalid)::tailList) funValue
            else
                if rv < constr then 
                    if rv > funv then mymap f ay value (parentFlag, maximizeFlag) ((i, j, Num rv)::tailList) (Num rv) 
                    else mymap f ay value (parentFlag, maximizeFlag) ((i, j, Num rv)::tailList) (Num funv) 
                else []
        | (true, Invalid, _, _) ->
            mymap f ay value (parentFlag, maximizeFlag) ((0, 0, Invalid)::tailList) funValue
        | (false, Num rv, AnyValue, AnyValue) -> 
            mymap f ay value (parentFlag, maximizeFlag) ((i, j, Num rv)::tailList) (Num rv)
        | (false, Num rv, AnyValue, Num funv) -> 
            if rv < funv then mymap f ay value (parentFlag, maximizeFlag) ((i, j, Num rv)::tailList) (Num rv) 
            else mymap f ay value (parentFlag, maximizeFlag) ((i, j, Num rv)::tailList) (Num funv) 
        | (false, Num rv, Num constr, AnyValue) -> 
            if parentFlag then
                if rv > constr then mymap f ay value (parentFlag, maximizeFlag) ((i, j, Num rv)::tailList) funValue else []
            else
                if rv < constr then 
                    mymap f ay value (parentFlag, maximizeFlag) ((i, j, Num rv)::tailList) funValue 
                else 
                    mymap f ay value (parentFlag, maximizeFlag) ((0, 0, Invalid)::tailList) funValue
        | (false, Num rv, Num constr, Num funv) -> 
            if parentFlag then
                if rv > constr then 
                    if rv < funv then mymap f ay value (parentFlag, maximizeFlag) ((i, j, Num rv)::tailList) (Num rv) 
                    else mymap f ay value (parentFlag, maximizeFlag) ((i, j, Num rv)::tailList) (Num funv) 
                else []
            else
                if rv < constr then 
                    if rv < funv then mymap f ay value (parentFlag, maximizeFlag) ((i, j, Num rv)::tailList) (Num rv) 
                    else mymap f ay value (parentFlag, maximizeFlag) ((i, j, Num rv)::tailList) (Num funv) 
                else 
                    mymap f ay value (parentFlag, maximizeFlag) ((0, 0, Invalid)::tailList) funValue
        | (false, Invalid, _, _) -> 
            mymap f ay value (parentFlag, maximizeFlag) ((0, 0, Invalid)::tailList) funValue
        | (_, _, _, _) -> print_string "(maximizeFlag, return_value, value, funValue) = "; 
                          print_string "("; print_string (string_of_bool maximizeFlag);
                          print_string ", "; print_value return_value;
                          print_string ", "; print_value value;
                          print_string ", "; print_value funValue;
                          print_string ") match error in mymap...\n"; raise Error

(*************************************************************************************************************************************************)
(* minimaxで用いるminとmax *)
(*************************************************************************************************************************************************)

let rec minScore lst minTuple ms =
    match (lst, minTuple, ms) with
    | ([], (i, j, value), []) -> (i, j, value)
    | ((i, j, value)::ay, (mini, minj, minValue), (msi, msj)::msy) ->
        (match (value, minValue) with
        | (Num v, Invalid) -> 
            minScore ay (msi, msj, Num v) msy
        | (Num v, Num min) ->
            if value > minValue then minScore ay (mini, minj, Num min) msy
            else minScore ay (msi, msj, Num v) msy
        | (_, a) ->
            minScore ay (mini, minj, a) msy)
    | (_, _, _) -> print_string "match error in minScore...\n"; raise Error

let rec maxScore lst maxTuple ms =
    match (lst, maxTuple, ms) with
    | ([], (i, j, value), []) -> (i, j, value)
    | ((i, j, value)::ay, (maxi, maxj, maxValue), (msi, msj)::msy) ->
        (match (value, maxValue) with
        | (Num v, Invalid) -> 
            maxScore ay (msi, msj, Num v) msy
        | (Num v, Num max) ->
            if value < maxValue then maxScore ay (maxi, maxj, maxValue) msy
            else maxScore ay (msi, msj, value) msy
        | (_, a) -> 
            maxScore ay (maxi, maxj, a) msy)
    | (_, _, _) -> print_string "match error in maxScore...\n"; raise Error

(*************************************************************************************************************************************************)
(* 中盤のminimax法 *)
(*************************************************************************************************************************************************)
(* (i, j, length)を1個返す *)

let rec midminimaxScore color board mycolor count value (i, j) =
    let oBoard = doMove (copyBoard board) (Mv (i, j)) color in (* (i, j)にcolorを置いたときの盤面*)
    let ms = valid_moves oBoard color in (* 新しい盤面でのcolorの有効手 *)
    let oms = valid_moves oBoard (opposite_color color) in (* 新しい盤面での相手のcolorの有効手 *)
    if count = 0 then
        (i, j, Num (-(List.length oms)))
    else
        match (ms, oms) with
        (* お互いパスで、試合終了のとき *)
        | ([], []) ->
            (i, j, Num 0)
        (* 次の人がパスのとき、つまり、再び同じ人の手番のとき *)
        | (a, []) ->
            if color = mycolor then (* 今の人の手番が自分で、次の人の手番として、再び自分が打つとき *)
                let msScoreList = mymap (midminimaxScore color oBoard mycolor (count - 1)) ms value (true, true) [] AnyValue in
                (match msScoreList with
                | [] -> (0, 0, Invalid) (* mymapで棄却されたとき、返す値は、上がminなので、infinityを返す *)
                | _ -> maxScore msScoreList (0, 0, Invalid) ms)
            else (* 今の人の手番が相手で、次の人の手番として、再び相手が打つとき *)
                let msScoreList = mymap (midminimaxScore color oBoard mycolor (count - 1)) ms value (false, false) [] AnyValue in
                (match msScoreList with
                | [] -> (0, 0, Invalid) (* mymapで棄却されたとき、返す値は、上がmaxなので、minus infinityを返す *)
                | _ -> minScore msScoreList (0, 0, Invalid) ms)
        (* 順当に次の人の手番のとき *)
        | (_, a) ->
            if color = mycolor then (* 今の人の手番が自分で、次の人の手番として、相手が打つとき *)
                let msScoreList = mymap (midminimaxScore (opposite_color color) oBoard mycolor (count - 1)) oms value (true, false) [] AnyValue in
                (match msScoreList with
                | [] -> (0, 0, Invalid) (* mymapで棄却されたとき、返す値は、上がmaxなので、minus infinityを返す *)
                | _ -> minScore msScoreList (0, 0, Invalid) oms)
            else (* 今の人の手番が相手で、次の人の手番として、自分が打つとき *)
                let msScoreList = mymap (midminimaxScore (opposite_color color) oBoard mycolor count) oms value (false, true) [] AnyValue in
                (match msScoreList with
                | [] -> (0, 0, Invalid) (* mymapで棄却されたとき、返す値は、上がminなので、infinityを返す *)
                | _ -> maxScore msScoreList (0, 0, Invalid) oms)

let rec corner ms msScoreList = 
    match (ms, msScoreList) with
    | ([], []) -> []
    | ((i, j)::msy, (is, js, value)::scorey) ->
        if (i, j) = (1, 2) || (i, j) = (2, 2) || (i, j) = (2, 1) || 
           (i, j) = (7, 8) || (i, j) = (7, 7) || (i, j) = (8, 7) || 
           (i, j) = (1, 7) || (i, j) = (2, 8) || (i, j) = (2, 7) || 
           (i, j) = (7, 1) || (i, j) = (8, 2) || (i, j) = (7, 2) then
               (match value with
               | Num v -> (is, js, Num (v - 6 + (Random.int 5) - 3))::(corner msy scorey)
               | Invalid | AnyValue -> (is, js, Invalid)::(corner msy scorey))
        else if (i, j) = (1, 1) || (i, j) = (8, 8) || (i, j) = (8, 1) || (i, j) = (1, 8) then
               (match value with
               | Num v -> (is, js, Num (v + 20 + (Random.int 5) - 3))::(corner msy scorey)
               | Invalid | AnyValue -> (is, js, Invalid)::(corner msy scorey))
        else
            (match value with
            | Num v -> (is, js, Num (v + (Random.int 5) - 3))::(corner msy scorey)
            | Invalid | AnyValue -> (is, js, Invalid)::(corner msy scorey))
    | (_, _) -> print_string "match error in corner...\n"; raise Error

let rec midminimax board ms color =
    print_string "in midminimax "; print_int_int_list ms; print_string "\n";
    match ms with
    | [] -> Pass
    | _ ->
        let msScoreList = mymap (midminimaxScore color board color 2) ms AnyValue (true, true) [] AnyValue in
        let scoreList = corner ms msScoreList in
        (match scoreList with
        | [] -> print_string "Nil list in midminimax...\n"; raise Error
        | _ -> 
            print_int_int_int_list scoreList;
            let (i, j, value) = maxScore scoreList (0, 0, Invalid) ms in
            print_int_int_list [(i, j)];
            print_string "=========================================================================================\n";
            Mv (i, j))

(*************************************************************************************************************************************************)
(* 終盤のminimax法 *)
(*************************************************************************************************************************************************)


(* (i, j, value)を1個返す *)
let rec minimaxScore color board mycolor value (i, j) =
    let oBoard = doMove (copyBoard board) (Mv (i, j)) color in (* (i, j)にcolorを置いたときの盤面*)
    let ms = valid_moves oBoard color in (* 新しい盤面でのcolorの有効手 *)
    let oms = valid_moves oBoard (opposite_color color) in (* 新しい盤面での相手のcolorの有効手 *)
    match (ms, oms) with
    (* お互いパスで、試合終了のとき *)
    | ([], []) ->
        let num = count oBoard mycolor in
        let onum = count oBoard (opposite_color mycolor) in
        (i, j, Num (num - onum))
    (* 次の人がパスのとき、つまり、再び同じ人の手番のとき *)
    | (a, []) ->
        if color = mycolor then (* 今の人の手番が自分で、次の人の手番として、再び自分が打つとき *)
            let msScoreList = mymap (minimaxScore color oBoard mycolor) ms value (true, true) [] AnyValue in
            (match msScoreList with
            | [] -> (0, 0, Invalid) (* mymapで棄却されたとき、返す値は、上がminなので、infinityを返す *)
            | _ -> 
                    maxScore msScoreList (0, 0, Invalid) ms)
        else (* 今の人の手番が相手で、次の人の手番として、再び相手が打つとき *)
            let msScoreList = mymap (minimaxScore color oBoard mycolor) ms value (false, false) [] AnyValue in
            (match msScoreList with
            | [] -> (0, 0, Invalid) (* mymapで棄却されたとき、返す値は、上がmaxなので、minus infinityを返す *)
            | _ -> 
                    minScore msScoreList (0, 0, Invalid) ms)
    (* 順当に次の人の手番のとき *)
    | (_, a) ->
        if color = mycolor then (* 今の人の手番が自分で、次の人の手番として、相手が打つとき *)
            let msScoreList = mymap (minimaxScore (opposite_color color) oBoard mycolor) oms value (true, false) [] AnyValue in
            (match msScoreList with
            | [] -> (0, 0, Invalid) (* mymapで棄却されたとき、返す値は、上がmaxなので、minus infinityを返す *)
            | _ -> 
                    minScore msScoreList (0, 0, Invalid) oms)
        else (* 今の人の手番が相手で、次の人の手番として、自分が打つとき *)
            let msScoreList = mymap (minimaxScore (opposite_color color) oBoard mycolor) oms value (false, true) [] AnyValue in
            (match msScoreList with
            | [] -> (0, 0, Invalid) (* mymapで棄却されたとき、返す値は、上がminなので、infinityを返す *)
            | _ -> 
                    maxScore msScoreList (0, 0, Invalid) oms)

let rec minimax board ms color =
    print_string "in minimax "; print_int_int_list ms; print_string "\n";
    match ms with
    | [] -> Pass
    | _ ->
        let msScoreList = mymap (minimaxScore color board color) ms AnyValue (true, true) [] AnyValue in
        print_string "the tailList (in minimax) is : "; print_int_int_int_list msScoreList;
        (match msScoreList with
        | [] -> print_string "Nil list in minimax\n"; raise Error
        | _ ->
            print_int_int_int_list msScoreList;
            let (i, j, value) = maxScore msScoreList (0, 0, Invalid) ms in
            print_int_int_list [(i, j)];
            print_string "=========================================================================================\n";
            Mv (i, j))

(*************************************************************************************************************************************************)
(* 四隅に置けるなら置くようにする *)
(*************************************************************************************************************************************************)

let rec isExistCorner ms =
    match ms with
    | [] -> (false, (0, 0))
    | (i, j)::yd ->
        if (i, j) = (1, 1) then (true, (1, 1))
        else if (i, j) = (1, 8) then (true, (1, 8))
        else if (i, j) = (8, 1) then (true, (8, 1))
        else if (i, j) = (8, 8) then (true, (8, 8))
        else isExistCorner yd

(*************************************************************************************************************************************************)
(* 操作本編 *)
(*************************************************************************************************************************************************)

(* どのセルにコマを置くか *)
let play board color = 
    print_board board;
    let ms = valid_moves board color in 
    print_int_int_list ms;
    if ms = [] then 
        Pass 
    else
        if (countNone board) > 11 then
            let (boolean, (i, j)) = isExistCorner ms in
            if boolean then
                (print_int_int_list [(i, j)];
                Mv (i, j))
            else
                (*let k = Random.int (List.length ms) in 
                let (i,j) = List.nth ms k in Mv (i,j) *)
                (print_string "call midminimax\n";
                midminimax board ms color)
        else
            let (boolean, (i, j)) = isExistCorner ms in
            if boolean then
                (print_int_int_list [(i, j)];
                Mv (i, j))
            else
                (print_string "call minimax\n";
                minimax board ms color)

let report_result board = 
    let _ = print_endline "========== Final Result ==========" in 
    let bc = count board black in 
    let wc = count board white in 
        if bc > wc then 
            print_endline "*Black wins!*" 
        else if bc < wc then 
            print_endline "*White wins!*" 
        else
            print_endline "*Even*";
        print_string "Black: "; print_endline (string_of_int bc);
        print_string "White: "; print_endline (string_of_int wc);
        print_board board 

