let twice f x = f (f x);;

let rec repeat f n x = if n = 1 then f x else repeat f (n - 1) (f x);;
